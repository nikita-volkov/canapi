module Canapi.ByType
where

import Canapi.Prelude
import Canapi.Data


data ByType a = ByType {
    text :: a,
    html :: a,
    xml :: a,
    json :: a,
    yaml :: a,
    binary :: a
  }

deriving instance Functor ByType

instance Applicative ByType where
  pure a = ByType a a a a a a
  (<*>) (ByType a b c d e f) (ByType g h i j k l) = ByType (a g) (b h) (c i) (d j) (e k) (f l)

get :: Type -> ByType a -> a
get = \ case
  TextType -> text
  HtmlType -> html
  XmlType -> xml
  JsonType -> json
  YamlType -> yaml
  BinaryType -> binary

justJson :: a -> ByType (Maybe a)
justJson a = ByType Nothing Nothing Nothing (Just a) Nothing Nothing

justYaml :: a -> ByType (Maybe a)
justYaml a = ByType Nothing Nothing Nothing Nothing (Just a) Nothing
