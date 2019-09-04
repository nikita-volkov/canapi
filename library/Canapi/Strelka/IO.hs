module Canapi.Strelka.IO where

import Canapi.Prelude
import Canapi.Strelka.Types
import qualified Optima
import qualified Canapi.Strelka.Optima.ParamGroup as Optima
import qualified Canapi.Strelka.Managed as Managed


serve :: Word16 -> Managed env -> Api env -> IO ()
serve port envManaged api = runManaged $ Managed.serve port envManaged api

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Text -> Optima.ParamGroup (Managed env) -> Api env -> IO ()
serveParsingCliArgs appDesc envParamGroup api = do
  (port, envManaged) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve port envManaged api
