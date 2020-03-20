module Canapi.Data
where

import Canapi.Prelude


{-|
Parsed request metadata reusable across alternative endpoints.
-}
data RequestMetadata = RequestMetadata {
    ip :: IP,
    userAgent :: Maybe Text,
    referer :: Maybe Text,
    contentType :: Maybe Type,
    acceptPreference :: [Type]
  }

{-|
Type supported by Canapi.
-}
data Type =
  TextType |
  HtmlType |
  XmlType |
  JsonType |
  YamlType |
  BinaryType

{-|
Request headers of interest to this library.
-}
data HeadersOfInterest = HeadersOfInterest {
    contentType :: Maybe ByteString,
    accept :: Maybe ByteString
  }
