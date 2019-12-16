module Canapi.IO where

import Canapi.Prelude
import Canapi.Types
import qualified Optima
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Canapi.Strelka.IO as StrelkaIO


serve :: Word16 -> Provider Text env -> Api env -> IO ()
serve = StrelkaIO.serve

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Text -> Optima.ParamGroup envArgs -> (envArgs -> Provider Text env) -> Api env -> IO ()
serveParsingCliArgs appDesc envParamGroup provider api = do
  (port, envArgs) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve port (provider envArgs) api
