module Canapi.HttpAuthorizationParsing
where

import Canapi.Prelude
import qualified Data.ByteString.Base64 as A
import qualified Data.Text as B
import qualified Data.Text.Encoding as B
import qualified Data.ByteString as C


basicCredentials :: ByteString -> Either Text (Text, Text)
basicCredentials =
  dropPrefix >=> decodeBase64 >=> decodeText >=> splitText
  where
    dropPrefix =
      maybe (Left "Not a basic authorization") Right .
      C.stripPrefix "Basic "
    decodeBase64 =
      first adaptFailure .
      A.decode
      where
        adaptFailure string =
          "Base64 decoding failure: " <> fromString string
    decodeText =
      first adaptFailure .
      B.decodeUtf8'
      where
        adaptFailure failure =
          "UTF8 decoding failure. " <>
          (B.pack . show) failure
    -- FIXME: How do we handle the absence of a password?
    splitText input =
      case B.span (/= ':') input of
        (prefix, remainder) ->
          if B.null prefix
            then Left ("Couldn't split the decoded text: " <> remainder)
            else if B.null remainder
              then Left ("Couldn't split the decoded text: " <> prefix)
              else Right (prefix, B.tail remainder)
