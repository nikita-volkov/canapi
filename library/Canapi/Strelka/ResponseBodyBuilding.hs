module Canapi.Strelka.ResponseBodyBuilding where

import Canapi.Prelude hiding (fail)
import Strelka.ResponseBodyBuilding
import qualified Data.Serialize.Put as Cereal


serialize :: Cereal.Put -> Builder
serialize = Cereal.runPutMBuilder >>> snd >>> lazyBytesBuilder
