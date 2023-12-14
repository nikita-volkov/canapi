module Canapi.HttpStatus where

import Canapi.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types.Status

ok :: Text -> Status
ok = textual 200

accepted :: Text -> Status
accepted = textual 202

temporaryRedirect :: Text -> Status
temporaryRedirect = textual 307

badRequest :: Text -> Status
badRequest = textual 400

unauthorized :: Text -> Status
unauthorized = textual 401

notFound :: Text -> Status
notFound = textual 404

methodNotAllowed :: Text -> Status
methodNotAllowed = textual 405

notAcceptable :: Text -> Status
notAcceptable = textual 406

unsupportedMediaType :: Text -> Status
unsupportedMediaType = textual 415

internalServerError :: Text -> Status
internalServerError = textual 500

textual :: Int -> Text -> Status
textual code = Status code . Text.encodeUtf8 . Text.filter (\a -> isAscii a && isPrint a)
