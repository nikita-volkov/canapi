module Canapi.IO where

import Canapi.Prelude
import Canapi.Types
import qualified Optima
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Canapi.Strelka.IO as StrelkaIO
import qualified Acquire


serve :: Word16 -> Acquire env -> Api env -> IO ()
serve = StrelkaIO.serve

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Text -> Optima.ParamGroup (Acquire env) -> Api env -> IO ()
serveParsingCliArgs appDesc envParamGroup api = do
  (port, envAcquire) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve port envAcquire api
