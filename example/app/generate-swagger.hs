module Main where

import Control.Lens
import Data.Swagger (version, info)
import Rapid.Swagger (exportSwagger)
import Server (api)


main = exportSwagger api mods "swagger.json"
  where
    mods = info . version .~ "0.01" -- Version needs to be set!
