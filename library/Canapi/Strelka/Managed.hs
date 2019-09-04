module Canapi.Strelka.Managed where

import Canapi.Prelude
import Canapi.Strelka.Types
import qualified Strelka.WAI as Wai


serve :: Word16 -> Managed env -> Api env -> Managed ()
serve port envManaged api = do
  env <- envManaged
  liftIO $ Wai.strelkaServer (fromIntegral port) (fmap Right . flip runReaderT env) api
