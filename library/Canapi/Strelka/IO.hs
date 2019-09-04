module Canapi.Strelka.IO where

import Canapi.Prelude
import qualified Acquire
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.WAI as Wai


serve :: Word16 -> Acquire env -> RequestParsing.Parser (Use env Text) ResponseBuilding.Builder -> IO ()
serve port envAcquire =
  Wai.strelkaServer
    (fromIntegral port)
    (Acquire.acquireAndUse envAcquire)
