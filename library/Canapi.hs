module Canapi (
  ) where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi.Data
import qualified Canapi.RequestAccessor as RequestAccessor
import qualified Canapi.ByType as ByType
import qualified Canapi.Response as Response
import qualified Canapi.MimeTypeList as MimeTypeList
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified WaiAppStatic.Types as WaiStatic
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Attoparsec.Data as AttoparsecData
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as HashMap


data Resource env segments =
  AtResource Text [Resource env segments] |
  forall segment. ByResource [SegmentParser segment] [Resource env (segment, segments)] |
  forall request response. EndpointResource Text (Receiver request) (Responder response) (segments -> request -> Fx env Err response)

data Receiver a

data Responder a

data SegmentParser a = SegmentParser Text (Attoparsec.Parser a)

data Err =
  ClientErr Text |
  ServerErr Text

at :: Text -> [Resource env segments] -> Resource env segments
at = AtResource

by :: [SegmentParser segment] -> [Resource env (segment, segments)] -> Resource env segments
by = ByResource

head :: (segments -> Fx env Err ()) -> Resource env segments
head handler = EndpointResource "head" (pure ()) mempty (\ segments () -> handler segments)

get :: Responder response -> (segments -> Fx env Err response) -> Resource env segments
get responder handler = EndpointResource "get" (pure ()) responder (\ segments () -> handler segments)

post :: Receiver request -> Responder response -> (segments -> request -> Fx env Err response) -> Resource env segments
post = EndpointResource "post"

put :: Receiver request -> Responder response -> (segments -> request -> Fx env Err response) -> Resource env segments
put = EndpointResource "put"

delete :: Responder response -> (segments -> Fx env Err response) -> Resource env segments
delete responder handler = EndpointResource "delete" (pure ()) responder (\ segments () -> handler segments)

instance Functor Receiver where
  fmap = error "TODO"

instance Applicative Receiver where
  pure = error "TODO"
  (<*>) = error "TODO"

instance Semigroup (Responder response) where
  (<>) = error "TODO"

instance Monoid (Responder response) where
  mempty = error "TODO"
  mappend = (<>)

instance Contravariant Responder where
  contramap = error "TODO"

instance Divisible Responder where
  conquer = error "TODO"
  divide = error "TODO"


-- * Demo
-------------------------

data Env

data Language

data Artifacts

data Name

root :: [Resource Env ()]
root = [
    at "groups" [],
    at "rpc" [
      by [nameParser] [
        by [nameParser] [
          put validatedRpcSchemaSourceReceiver conquer (uncurryH putRpcSchemaHandler),
          get (error "TODO") (error "TODO"),
          at "artifacts" [
            get (error "TODO") (error "TODO"),
            by [languageParser] [
              get artifactsResponder (uncurryH getLanguageArtifacts)
            ]
          ]
        ]
      ]
    ]
  ]

nameParser :: SegmentParser Name
nameParser = error "TODO"

languageParser :: SegmentParser Language
languageParser = error "TODO"

validatedRpcSchemaSourceReceiver :: Receiver ByteString
validatedRpcSchemaSourceReceiver = error "TODO"

artifactsResponder :: Responder Artifacts
artifactsResponder = error "TODO"

putRpcSchemaHandler :: Name -> Name -> ByteString -> Fx env Err ()
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Language -> Name -> Name -> Fx Env Err Artifacts
getLanguageArtifacts = error "TODO"
