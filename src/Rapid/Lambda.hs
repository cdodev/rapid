{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Marshalling between API Gateway and Servant services
--
-- Example usage
--
-- @
--     import Aws.Lambda
--     import qualified Server (app)
--     import qualified Rapid.Lambda (handler)
--
--     main :: IO ()
--     main = do
--       myApp <- Server.app
--       runLambda $ run myApp
--       where
--         run app LambdaOptions {functionHandler, contextObject, eventObject, executionUuid }
--           = case functionHandler of
--             "my-handler" ->  runHander (Rapid.Lambda.handler app)
--             _ -> error "No handler"
--           where
--             runHander f = either (Left . encodeObj) (Right . LambdaResult . encodeObj)
--                       <$> f (decodeObj eventObject) (decodeObj contextObject)
-- @
module Rapid.Lambda (handler) where

import qualified Aws.Lambda as Lambda
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import qualified Data.Binary.Builder as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IP as IP
import Data.IP (IP)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as Vault
import GHC.Generics
import qualified Network.HTTP.Types as H
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
-- import Network.Wai.Handler.Lambda
import qualified Network.Wai.Internal as Wai
import Text.Read (readMaybe)
import UnliftIO

--------------------------------------------------------------------------------
-- | Needed for a sane 'FromJSON' instance for 'V4GatewayRequest'
data RequestIdentity
  = RequestIdentity
      {sourceIp :: !IP}
  deriving (Eq, Show, Generic)

-- | Parser from 'Read' instance
readParse
  :: Read a
  => String -- ^ Error messagddock e
  -> Text -- ^ Parser input (passed to 'readMaybe')
  -> Parser a
readParse msg str =
  case readMaybe (T.unpack str) of
    Just result -> pure result
    Nothing -> fail $ "Failed to parse an " ++ msg ++ (T.unpack str)

instance FromJSON RequestIdentity where
  parseJSON =
    withObject "RequestIdentity" $ \o ->
      RequestIdentity
        <$> (o .: "sourceIp" >>= \src -> (readParse "IP address" src) <|> readParse "localhost" "127.0.0.1")

-- | Needed for a sane 'FromJSON' instance for 'V4GatewayRequest'
data ProxyRequestContext
  = ProxyRequestContext
      { identity :: !RequestIdentity
        -- , _prcResourcePath :: !Text
        -- , _prcHttpMethod   :: !Text
        -- , _prcApiId        :: !Text
        -- , _prcProtocol     :: !Text
        -- , _prcAuthorizer   :: !(Maybe Authorizer)
        }
  deriving (Eq, Show, Generic, FromJSON)

-- | API Gateway request object
--
-- This is what API Gateway passes to the lambda function
data V4GatewayRequest
  = V4GatewayRequest
      { resource :: Text,
        path :: Text,
        httpMethod :: Text,
        headers :: Maybe (HashMap Text Text),
        queryStringParameters :: Maybe (HashMap Text Text),
        pathParameters :: Maybe (HashMap Text Text),
        stageVariables :: Maybe (HashMap Text Text),
        requestContext :: ProxyRequestContext,
        body :: Maybe Text
        }
  deriving (Eq, Show, Generic, FromJSON)

maybeHMToList :: Maybe (HashMap k v) -> [(k, v)]
maybeHMToList = maybe [] HashMap.toList

-- | Convert from the object that API Gateway gives the lambda function to one that servant accepts
toWAIRequest :: V4GatewayRequest -> IO Wai.Request
toWAIRequest V4GatewayRequest {..} = do
  let path' = T.encodeUtf8 path
      -- "httpMethod": "GET"
      requestMethod = T.encodeUtf8 httpMethod
      -- We don't get data about the version, just assume
      httpVersion = H.http11
      -- "queryStringParameters": {
      --    "name": "me"
      --  },
      -- XXX: default to empty object for the query params as Lambda doesn't set
      -- 'queryStringParameters' if there are no query parameters
      queryParams = bimap T.encodeUtf8 T.encodeUtf8 <$> maybeHMToList queryStringParameters
      rawQueryString = H.renderSimpleQuery True queryParams
      -- "path": "/test/hello",
      rawPathInfo = path' <> rawQueryString
      --  "headers": {
      --    "Accept": "text/html,application/xhtml+xml,...",
      --    ...
      --    "X-Forwarded-Proto": "https"
      --  },
      requestHeaders = bimap (CI.mk . T.encodeUtf8) T.encodeUtf8 <$> maybeHMToList headers
      isSecure = case lookup "X-Forwarded-Proto" requestHeaders of
        Just "https" -> True
        _ -> False
      --  "requestContext": {
      --    ...
      --    "identity": {
      --      ...
      --      "sourceIp": "192.168.100.1",
      --    },
      --    ...
      --  },
      ip = sourceIp $ identity requestContext
      remoteHost = case ip of
        IP.IPv4 ip4 ->
          Socket.SockAddrInet
            0 -- default port
            (IP.toHostAddress ip4)
        IP.IPv6 ip6 ->
          Socket.SockAddrInet6
            0 -- default port
            0 -- flow info
            (IP.toHostAddress6 ip6)
            0 -- scope id
      pathInfo = H.decodePathSegments path'
      queryString = H.parseQuery rawQueryString
      -- XXX: default to empty body as Lambda doesn't always set one (e.g. GET
      -- requests)
      requestBodyRaw = maybe "" T.encodeUtf8 body
      requestBodyLength = Wai.KnownLength $ fromIntegral $ BS.length requestBodyRaw
      vault = Vault.empty
      requestHeaderHost = lookup "host" requestHeaders
      requestHeaderRange = lookup "range" requestHeaders
      requestHeaderReferer = lookup "referer" requestHeaders
      requestHeaderUserAgent = lookup "User-Agent" requestHeaders
  requestBodyMVar <- newMVar requestBodyRaw
  let requestBody = do
        tryTakeMVar requestBodyMVar >>= \case
          Just bs -> pure bs
          Nothing -> pure BS.empty
  pure Wai.Request {..}

-- | API Gateway response
--
-- This is what API Gateway expects back from the lambda function
data V4GatewayResponse
  = V4GatewayResponse
      { statusCode :: Int,
        headers :: HashMap Text Text,
        body :: Text
        }
  deriving (Eq, Show, Generic, ToJSON)

-- | Convert from the object that servant returns to one that the API Gateway expects from the lambda
toV4GatewayRespone :: Wai.Response -> IO V4GatewayResponse
toV4GatewayRespone (Wai.responseToStream -> (st, hdrs, mkBody)) = do
  body <- mkBody drainBody
  pure
    $ V4GatewayResponse
        (H.statusCode st)
        (HashMap.fromList $ bimap (T.decodeUtf8 . CI.original) T.decodeUtf8 <$> hdrs)
        body
  where
    drainBody :: Wai.StreamingBody -> IO Text
    drainBody body = do
      ioref <- newIORef Binary.empty
      body
        (\b -> atomicModifyIORef ioref (\b' -> (b <> b', ())))
        (pure ())
      T.decodeUtf8 . BL.toStrict . Binary.toLazyByteString <$> readIORef ioref

-- | Calls the 'Wai.Application' converting request and response types from API Gateway
handler
  :: Wai.Application -- ^ Application from servant API
  -> V4GatewayRequest -- ^ Payload from API Gateway
  -> Lambda.Context -- ^ Lambda context
  -> IO (Either String V4GatewayResponse)
handler app gw _context = do
  resp <- tryAny (processRequest app =<< toWAIRequest gw)
  case resp of
    Right r -> do
      apiResp <- toV4GatewayRespone r
      -- TODO proper structured logging here
      -- putStrLn $ show apiResp
      pure $ Right apiResp
    Left e -> pure $ Left $ show e

processRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
processRequest app req = do
    mvar <- newEmptyMVar
    Wai.ResponseReceived <- app req $ \resp -> do
      putMVar mvar resp
      pure Wai.ResponseReceived
    takeMVar mvar
