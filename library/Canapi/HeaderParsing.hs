module Canapi.HeaderParsing where

import Canapi.Data
import qualified Canapi.MimeTypeList as MimeTypeList
import Canapi.Prelude
import qualified Network.HTTP.Media as HttpMedia

contentType :: ByteString -> Maybe Type
contentType = mapper HttpMedia.mapContentMedia

accept :: ByteString -> Maybe Type
accept = mapper HttpMedia.mapAcceptMedia

mapper :: ([(HttpMedia.MediaType, Type)] -> t) -> t
mapper fn = fn mappings
  where
    mappings =
      mconcat
        [ def TextType MimeTypeList.text,
          def HtmlType MimeTypeList.html,
          def XmlType MimeTypeList.xml,
          def JsonType MimeTypeList.json,
          def YamlType MimeTypeList.yaml,
          def BinaryType MimeTypeList.binary
        ]
      where
        def a b = fmap (,a) b
