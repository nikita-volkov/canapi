module Canapi.Parser where

import Canapi.Prelude
import Canapi.Types
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.RequestBodyParsing as RequestBodyParsing


action :: (env -> actionEnv) -> (error -> Text) -> Action actionEnv error result -> Parser env result
action envMapping errorMapping action = do
  either <- lift $ withReaderT envMapping $ mapReaderT runExceptT $ action
  case either of
    Left error -> RequestParsing.fail (errorMapping error)
    Right result -> return result
