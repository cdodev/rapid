{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Rapid.Swagger where
--   ( StaticDoc,
--     swaggerSchemaUIServer,
--     mkSwagger,
--     staticDoc,
--     exportSwagger,
--     XAmazonGatewayIntegrationEndpoint(..),
--     mkGatewayStanza
--     )
-- where

import Control.Exception (handleJust)
import Control.Lens hiding ((.=))
import Data.Aeson

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Generics
import qualified Language.Haskell.TH as TH
import NeatInterpolation (text)
import Servant.Server (Handler, Server)
import Servant.Swagger
import Servant.Swagger.UI (SwaggerSchemaUI)
import Servant.Swagger.UI.Core (SwaggerSchemaUI', swaggerSchemaUIServerImpl)
import Servant.Swagger.UI.ReDoc (redocFiles)
import System.Exit (ExitCode (..))
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import System.Process (readProcessWithExitCode)

----------------------------------------------------------------------------------------------------
-- | Swagger UI type
type StaticDoc = SwaggerSchemaUI "docs" "swagger.json"

-- | Extra Documentation fields
data SwaggerMetaData = SwaggerMetaData {
    _swaggerTitle :: Maybe Text
  , _swaggerVersion :: Maybe Text
  , _swaggerHost :: Maybe Text
  , _swaggerDescription :: Maybe Text
  } deriving (Show)

----------------------------------------------------------------------------------------------------
-- | Extra json needed for AWS lambda see 'addAWSGatewayStanza' and 'mkGatewayStanza'
--
-- This is required for the terraform @servant_api_swagger@ template
--
-- ** Example:
--
-- @
--
-- "x-amazon-apigateway-integration":
--     httpMethod: "POST",
--     passthroughBehavior: "when_no_match",
--     uri: "${servant_lambda_arn}",
--     type: "aws_proxy"
-- @
data XAmazonGatewayIntegrationEndpoint
  = GW
      { uri :: Text, -- ^ uri field. This is templated for replacement by terraform
        passthroughBehavior :: Text, -- ^ set to @when_no_match@ in 'mkGatewayStanza'
        httpMethod :: Text, -- ^ HTTP request method (@POST@, @GET@ etc)
        type_ :: Text -- ^ The type field. Set tp @aws_proxy@
      }
  deriving (Show, Eq, Generic)

instance ToJSON XAmazonGatewayIntegrationEndpoint where
  toJSON (GW uri pt method typ) =
    object
      [ "uri" .= uri,
        "passthroughBehavior" .= pt,
        "httpMethod" .= method,
        "type" .= typ
        ]

-- | Smart constructor for 'XAmazonGatewayIntegration'
--
-- Needs to embed the arn in @${}@ for later processing by terraform
mkGatewayStanza
  :: Text -- ^ The HTTP Method
  -> Text -- ^ The arn that is referred to in the terraform module @${servant_lambda_arn}@
  -> XAmazonGatewayIntegrationEndpoint
mkGatewayStanza method arn =
  let arn' = "${" <> arn <> "}"
  in GW arn' "when_no_match" method "aws_proxy"

----------------------------------------------------------------------------------------------------
type TemplateContentType = Text

jsonCT :: TemplateContentType
jsonCT = "application/json"

----------------------------------------------------------------------------------------------------
-- Add responsese to the x-amazon-apigateway-integration options request
--
-- responses:
--   "default":
--     statusCode: "200"
--     responseParameters:
--       method.response.header.Access-Control-Allow-Headers : "'Content-Type,X-Amz-Date,Authorization,X-Api-Key'"
--       method.response.header.Access-Control-Allow-Methods : "'*'"
--       method.response.header.Access-Control-Allow-Origin : "'*'"
--     responseTemplates:
--       application/json: |
--         {}
data GatewayResponse = GatewayResponse {
    -- | HTTP status code that this response matches. Must be 200 for the CORS endpoint to work properly
    statusCode :: Int
    -- | response parameters are used in the resource's "Integration Response" section
  , responseParams :: HashMap Text Text
    -- | Response templates per content type. Default is json and {}
  , responseTemplates :: HashMap TemplateContentType Text
  } deriving (Show, Eq, Generic)

instance ToJSON GatewayResponse where
  toJSON (GatewayResponse stat respHdrs respTmpl) =
    object
      [ "statusCode" .= stat,
        "responseParameters" .= respHdrs,
        "responseTemplates" .= respTmpl
        ]

-- | Construct a 'GatewayResponse' to embed in 'XAmazonGatewayIntegrationCORs.responses'
mkGatewayResponse
  :: Int -- ^ HTTP status code (200)
  -> [Text] -- ^ Access-Control-Allow-Headers list of headers
  -> [Text] -- ^ Access-Control-Allow-Methods list of methods
  -> Text -- ^ Access-Control-Allow-Origin usually @"*"@
  -> GatewayResponse
mkGatewayResponse resp allowHeaders allowMethods allowOrigin =
  let respParams = HashMap.fromList [
          ("method.response.header.Access-Control-Allow-Headers", singleQuote $ Text.intercalate "," allowHeaders)
        , ("method.response.header.Access-Control-Allow-Methods", singleQuote $ Text.intercalate "," allowMethods)
        , ("method.response.header.Access-Control-Allow-Origin", singleQuote allowOrigin)
        ]
      respTemplates = HashMap.fromList [(jsonCT, "{}")]
      singleQuote s = "'" <> s <> "'"
  in GatewayResponse resp respParams respTemplates

defaultGatewayResponse :: GatewayResponse
defaultGatewayResponse = mkGatewayResponse 200 ["Content-Type", "X-Amz-Date", "Authorization", "X-Api-Key"] ["*"] "*"


--------------------------------------------------------------------------------
-- | Extra json needed for API Gateway CORs endpoints see 'addAWSGatewayCORs' and 'mkGatewayStanza'
--
-- This is required for the terraform @servant_api_swagger@ template
-- See @main.tf@ in the @terraform-module@
--
-- ** Example:
--
-- @
-- x-amazon-apigateway-integration:
--     type: mock
--     requestTemplates:
--       application/json: |
--         {
--           "statusCode" : 200
--         }
--     responses:
--       "default":
--         statusCode: "200"
--         responseParameters:
--           method.response.header.Access-Control-Allow-Headers : "'Content-Type,X-Amz-Date,Authorization,X-Api-Key'"
--           method.response.header.Access-Control-Allow-Methods : "'*'"
--           method.response.header.Access-Control-Allow-Origin : "'*'"
--         responseTemplates:
--           application/json: |
--             {}
-- @
data XAmazonGatewayIntegrationCORs
  = GWCORs
      { -- | type field will be @mock@
        type_ :: Text,
        -- | mapping of content type to response to return
        reqTemplates :: HashMap TemplateContentType Text,
        -- | Fills out headers in the "Integration Response" part of the resource
        responses :: HashMap Text GatewayResponse
        }
  deriving (Show, Eq, Generic)

instance ToJSON XAmazonGatewayIntegrationCORs where
  toJSON (GWCORs typ tmpl resp) =
    object [
        "type" .= typ,
        "requestTemplates" .= tmpl,
        "responses" .= toJSON resp
        ]

-- | Smart constructor for 'XAmazonGatewayIntegrationCORs'
mkGatewayStanzaCORS
  :: [Text] -- ^ Access-Control-Allow-Headers list of headers
  -> [Text] -- ^ Access-Control-Allow-Methods list of methods
  -> Text -- ^ Access-Control-Allow-Origin usually @"*"@
  -> XAmazonGatewayIntegrationCORs
mkGatewayStanzaCORS allowHeaders allowMethods allowOrigin =
  let reqTemplates = HashMap.fromList [(jsonCT, "{\"statusCode\": 200}")]
      defaultResp = mkGatewayResponse 200 allowHeaders allowMethods allowOrigin
  in GWCORs "mock" reqTemplates $ HashMap.fromList [("default", defaultResp)]

-- | Default (permissive) value for 'XAmazonGatewayIntegrationCORs'
defaultCorsStanza :: XAmazonGatewayIntegrationCORs
defaultCorsStanza =
  let reqTemplates = HashMap.fromList [(jsonCT, "{\"statusCode\": 200}")]
  in GWCORs "mock" reqTemplates $ HashMap.fromList [("default", defaultGatewayResponse)]

----------------------------------------------------------------------------------------------------
staticDoc :: Proxy StaticDoc
staticDoc = Proxy

-- | Git revision found by running git rev-parse. If git could not be
-- executed, then this will be an empty string.
-- TODO: Get this working again
_gitRevFromGit :: TH.Q TH.Exp
_gitRevFromGit = TH.LitE . TH.StringL <$> TH.runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = handleJust missingGit (const $ pure "") $ do
      (exitCode, output, _) <-
        readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
      pure $ case exitCode of
        ExitSuccess -> output
        _ -> ""
    missingGit e = if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing


--------------------------------------------------------------------------------
-- | Provide an alternative UI (ReDoc) for rendering Swagger documentation.
swaggerSchemaUIServer
  :: (Server api ~ Handler Swagger)
  => Swagger
  -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer =
  swaggerSchemaUIServerImpl redocIndexTemplate redocFiles
  where
    redocIndexTemplate :: Text
    redocIndexTemplate =
      [text|
<!doctype html>
<html lang="en">
  <head>
    <title>ReDoc</title>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      body { margin: 0; padding: 0; }
    </style>
    <script>
        // Force Strict-URL Routing for assets relative paths
        (function onload() {
            if (!window.location.pathname.endsWith("/")) {
                window.location.pathname += "/";
            }
        }());
    </script>
  </head>
  <body>
    <redoc spec-url="../SERVANT_SWAGGER_UI_SCHEMA"></redoc>
    <script src="https://cdn.jsdelivr.net/npm/redoc@next/bundles/redoc.standalone.js"></script>
  </body>
</html>|]


----------------------------------------------------------------------------------------------------
-- | Constructor for the API's 'Swagger' definition
mkSwagger
  :: HasSwagger a
  => Proxy a -- ^ The API
  -> (Swagger -> Swagger) -- ^ Any mods to be made
  -> Swagger -- ^ Swagger definition
mkSwagger staticAPI mods = mods $
  toSwagger staticAPI
    -- API Gateway doesn't accept the examples field
    --
    -- So we traverse over the examples and set them to Nothing
    & definitions . traversed . example .~ Nothing
    & definitions . traversed . properties . traversed . _Inline . example .~ Nothing

-- | Export the swagger from 'mkSwagger', adding the required 'XAmazonGatewayIntegration' info
--
-- This function is run as an executable in CI for terrafrom to pick up and use as a template
-- See
--
-- * https://www.terraform.io/docs/providers/aws/r/api_gateway_documentation_part.html
--
-- * https://ordina-jworks.github.io/cloud/2019/01/14/Infrastructure-as-code-with-terraform-and-aws-serverless.html
exportSwagger
  :: HasSwagger api
  => Proxy api -- ^ API to generate swagger for
  -> (Swagger -> Swagger) -- ^ Any mods to be made
  -> FilePath -- ^ Destination file to write the annotated swagger definition to
  -> IO ()
exportSwagger api mods path = do
  let apiJSON = addAWSGatewayStanza "servant_lambda_arn" $ toJSON (mkSwagger api mods)
  BSL.writeFile path $ encodePretty apiJSON

-- | Adds the required @x-amazon-apigateway-integration@ that terraform needs
--
-- See
-- * https://www.terraform.io/docs/providers/aws/r/api_gateway_documentation_part.html
addAWSGatewayStanza
  :: Text -- ^ The variable used in terraform @servant_lambda_arn@
  -> Value -- ^ Swagger json pre marking up with 'x-amazon-apigateway-integration' stanzas
  -> Value -- ^ Swagger with the right annotations to auto-create API Gateway resources
addAWSGatewayStanza terraformVariable v =
  -- Add the POST stanza for any post resources
  v & key "paths" . members . key "post" . _Object %~ (HashMap.insert "x-amazon-apigateway-integration" stanzaPOST)
    -- Add the GET stanza for any get requests
    & key "paths" . members . key "get" . _Object %~ (HashMap.insert "x-amazon-apigateway-integration" stanzaGET)
    -- Add the OPTIONS CORS stanza for any options resources
    & key "paths" . members . key "options" . _Object %~ (HashMap.insert "x-amazon-apigateway-integration" corsStanza)
  where
    stanzaGET = toJSON $ mkGatewayStanza "GET" terraformVariable
    stanzaPOST = toJSON $ mkGatewayStanza "POST" terraformVariable
    corsStanza = toJSON $ defaultCorsStanza
