module Canapi.Strelka.RequestBodyParsing where

import Canapi.Prelude hiding (fail)
import Strelka.RequestBodyParsing
import qualified Data.Serialize.Get as Cereal


deserialize :: Cereal.Get a -> Parser a
deserialize get =
  feedResult (Cereal.Partial (Cereal.runGetPartial get)) >>= \ case
    Cereal.Done result _ -> return result
    Cereal.Fail message _ -> fail (fromString message)
    Cereal.Partial _ -> fail "Not enough input"

feedResult :: Cereal.Result a -> Parser (Cereal.Result a)
feedResult = foldBytes $ \ case
  Cereal.Partial parse -> Unfinished . parse
  result -> const (Finished result)
