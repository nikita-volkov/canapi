module Canapi.Types where

import Canapi.Prelude
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Strelka.ResponseBodyBuilding as ResponseBodyBuilding


type Api env = Parser env ResponseBuilding.Builder

type Parser env = RequestParsing.Parser (Fx env Text)
