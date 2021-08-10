
import           Prelude

import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy as LB

import           Cardano.PlutusExample.ScriptContextChecker

main :: IO ()
main = do
  cmd <- getLine
  case cmd of
    "generate" -> LB.writeFile "example/work/script-context.redeemer" sampleTestScriptContextDataJSON
    txbodyfile -> do
      eTxBodyRedeemer <- runExceptT $ txBodytoRedeemer txbodyfile
      case eTxBodyRedeemer of
        Left err -> print err
        Right () -> return ()
