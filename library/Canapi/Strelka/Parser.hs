module Canapi.Strelka.Parser where

import Canapi.Prelude
import Canapi.Strelka.Types
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Strelka.WAI as Wai
import qualified Optima


action :: (env -> actionEnv) -> (error -> Text) -> Action actionEnv error result -> Parser env result
action envMapping errorMapping action = do
  either <- lift $ withReaderT envMapping $ mapReaderT runExceptT $ action
  case either of
    Left error -> RequestParsing.fail (errorMapping error)
    Right result -> return result
