module Canapi.Strelka.IO where

import Canapi.Prelude
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.WAI as Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.Wai.Handler.Warp as Warp


serve :: Word16 -> Bool -> Provider Text env -> RequestParsing.Parser (Fx env Text) ResponseBuilding.Builder -> IO ()
serve port cors provider =
  Warp.run (fromIntegral port) .
  (if cors then WaiCors.simpleCors else id) .
  Wai.strelkaApplication (runExceptT . runFx . provideAndUse provider)
