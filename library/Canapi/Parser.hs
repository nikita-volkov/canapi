module Canapi.Parser where

import Canapi.Prelude
import Canapi.Types
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Acquire


use :: (env -> useEnv) -> (err -> Text) -> Use useEnv err res -> Parser env res
use envMapping errMapping = lift . Acquire.mapEnvAndErr envMapping errMapping
