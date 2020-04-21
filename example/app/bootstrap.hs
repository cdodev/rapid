import Aws.Lambda

import qualified Server (app)
import qualified Rapid.Lambda (handler)

main :: IO ()
main = do
  runLambda $ run Server.app
  where
    run app LambdaOptions {functionHandler, contextObject, eventObject, executionUuid }
      = case functionHandler of
        "example" ->  runHander (Rapid.Lambda.handler app)
        _ -> error "No handler"
      where
        runHander f = either (Left . encodeObj) (Right . LambdaResult . encodeObj)
                  <$> f (decodeObj eventObject) (decodeObj contextObject)
