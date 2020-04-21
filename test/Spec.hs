{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Lens
import Rapid.Swagger
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec
import Example

-- main :: IO ()
main = do
  hspecs <- testSpecs spec
  gwSpecs <- testSpecs gatewayIntegrationSpec
  let tree = testGroup "All Tests"
                [ testGroup "Example API" hspecs
                , testGroup "API Gateway JSON" gwSpecs
                ]

  defaultMain tree


-- postJSONResp pth body = Aeson.parseMaybe parseJSON <$> post pth body

-- shouldRespondWithJSON resp match = shouldRespondWith (parseMaybe parseJSON <$> resp) match

spec :: Spec
spec = with (return app) $ do
    describe "POST /users" $ do
        it "responds with 200" $ do
            post "/users" "" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\",\"userId\":1},{\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\",\"userId\":2}]"
            post "/users" "" `shouldRespondWith` users


gatewayIntegrationSpec :: Spec
gatewayIntegrationSpec = do
  describe "GatewayResponse json" $ do
    it "has the right json structure" $ do
      encode defaultGatewayResponse `shouldBe` "{\"responseTemplates\":{\"application/json\":\"{}\"},\"statusCode\":200,\"responseParameters\":{\"method.response.header.Access-Control-Allow-Headers\":\"'Content-Type,X-Amz-Date,Authorization,X-Api-Key'\",\"method.response.header.Access-Control-Allow-Methods\":\"'*'\",\"method.response.header.Access-Control-Allow-Origin\":\"'*'\"}}"
  describe "XAmazonGatewayIntegration json" $ do
    describe "POST" $ do
      let gw = mkGatewayStanza "POST" "test"
          jsonGW = toJSON gw
      it "has when_no_match in passThroughBehaviour" $ do
        jsonGW ^? key "passthroughBehavior" . _String `shouldBe` Just "when_no_match"
      it "has escaped uri argument in uri field" $ do
        jsonGW ^? key "uri" . _String `shouldBe` Just "${test}"
      it "http_method is POST" $ do
        jsonGW ^? key "httpMethod" . _String `shouldBe` Just "POST"
      it "type is aws_proxy" $ do
        jsonGW ^? key "type" . _String `shouldBe` Just "aws_proxy"
      it "No requestTemplates or responses" $ do
        jsonGW ^? key "requestTemplates" `shouldBe` Nothing
        jsonGW ^? key "responses" `shouldBe` Nothing

-- swaggerGenTests :: TestTree
-- swaggerGenTests = testGroup "SwaggerGeneration"
--   [
--      testGroup
--   ]
