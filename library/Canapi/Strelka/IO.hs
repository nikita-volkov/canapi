module Canapi.Strelka.IO where

import Canapi.Prelude
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.WAI as Wai


serve :: Word16 -> Provider Text env -> RequestParsing.Parser (Fx env Text) ResponseBuilding.Builder -> IO ()
serve port provider = Wai.strelkaServer (fromIntegral port) (runExceptT . runFx . provideAndUse provider)
