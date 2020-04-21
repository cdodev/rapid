module Rapid.CORS where

import Data.Text (Text)
import Data.Text as Text
import GHC.Generics (Generic)
import Servant

----------------------------------------------------------------------------------------------------
newtype HeaderList = HeaderList { unHeaderList :: [Text] } deriving (Show, Generic)

headersToText :: HeaderList -> Text
headersToText = Text.intercalate "," . unHeaderList

----------------------------------------------------------------------------------------------------
type Opts = Verb 'OPTIONS 200

type CORSResponseHeaders = '[
    Header "Access-Control-Allow-Origin" Text
  , Header "Access-Control-Allow-Headers" Text
  , Header "Access-Control-Allow-Methods" Text
  , Header "Access-Control-Allow-Max-Age" Int
  ]

type family AddCors t where
  AddCors t =   Header "Access-Control-Request-Method" Text
             :> Header "Access-Control-Request-Headers" Text
             :> Opts '[JSON] (Headers CORSResponseHeaders NoContent)
             :<|> t

defaultRespHeaders, defaultMethods :: HeaderList
defaultRespHeaders = HeaderList ["Content-Type", "X-Amz-Date", "Authorization", "X-Api-Key"]
defaultMethods = HeaderList ["POST", "GET", "DELETE"]

----------------------------------------------------------------------------------------------------
addCorsResponseHeaders :: HeaderList -> HeaderList -> t -> Headers CORSResponseHeaders t
addCorsResponseHeaders respHeaders methods =
         addHeader "*"
       . addHeader (headersToText respHeaders)
       . addHeader (headersToText methods)
       . addHeader 86400

----------------------------------------------------------------------------------------------------
corsOptionsHandler :: HeaderList -> HeaderList -> Maybe Text -> Maybe Text -> Handler (Headers CORSResponseHeaders NoContent)
corsOptionsHandler respHeaders methods _requestMethodHdr _requestHeadersHdr =
  pure (addCorsResponseHeaders respHeaders methods NoContent)

----------------------------------------------------------------------------------------------------
addDefaultCorsHandler :: handler -> ((Maybe Text -> Maybe Text -> Handler (Headers CORSResponseHeaders NoContent)) :<|> handler)
addDefaultCorsHandler h = corsOptionsHandler defaultRespHeaders defaultMethods :<|> h
