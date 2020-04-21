{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}

module Server
    ( startApp
    , app
    , api
    ) where

import Control.Lens
import Data.Aeson
import Data.Swagger hiding (Header)
import Network.Wai
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Rapid.CORS
import Rapid.Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

type API = "users" :> AddCors (Post '[JSON] (Headers CORSResponseHeaders [User]))

startApp :: IO ()
startApp = run 8081 $ logStdoutDev $ serve withDocs server

api :: Proxy API
api = Proxy

withDocs :: Proxy WithDocs
withDocs = Proxy

app :: Application
app = logStdout $ serve withDocs server

type WithDocs = StaticDoc :<|> API

server :: Server WithDocs
server = swaggerSchemaUIServer swagger :<|> (addDefaultCorsHandler usersR)
  where
    usersR = pure $ addCorsResponseHeaders defaultRespHeaders defaultMethods users
    mods = info . version .~ "0.01" -- Version needs to be set!
    swagger = mkSwagger api mods

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
