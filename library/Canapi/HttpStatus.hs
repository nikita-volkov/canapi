module Canapi.HttpStatus where

import Canapi.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types.Status

ok = textual 200

accepted = textual 202

temporaryRedirect = textual 307

badRequest = textual 400

unauthorized = textual 401

notFound = textual 404

methodNotAllowed = textual 405

notAcceptable = textual 406

unsupportedMediaType = textual 415

internalServerError = textual 500

textual :: Int -> Text -> Status
textual code = Status code . Text.encodeUtf8 . Text.filter (\a -> isAscii a && isPrint a)
