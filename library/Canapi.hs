module Canapi (
    Resource,
    SegmentParser,
    Receiver,
    Responder,
    Realm,
    MediaType,
    Err(..),
    -- * Execution
    serve,
    buildWaiApplication,
    -- * Resource
    at,
    by,
    head,
    get,
    post,
    put,
    delete,
    authenticated,
    temporaryRedirect,
    -- * SegmentParser
    segment,
    -- * Receiver
    ofJsonAst,
    ofJsonBytes,
    ofYamlAst,
    ofYamlBytes,
    -- * Responder
    asJson,
    asYaml,
    asFile,
  ) where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi.Data
import qualified Canapi.Application as Application
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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Attoparsec.Data as AttoparsecData
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Foldl
import qualified Fx


data Resource env params =
  AtResource Text [Resource env params] |
  forall segment. ByResource (SegmentParser segment) [Resource env (segment, params)] |
  forall identity. AuthenticatedResource Realm (Text -> Text -> Fx env Err (Maybe identity)) [Resource env (identity, params)] |
  forall request response. HandlerResource HttpTypes.Method (Receiver request) (Responder response) (params -> request -> Fx env Err response) |
  RedirectResource Int (params -> Either Text Text) |
  FileSystemResource FilePath

data Receiver a = Receiver [(HttpMedia.MediaType, ByteString -> Either Text a)]

data Responder a = Responder [(HttpMedia.MediaType, a -> Wai.Response)]

data SegmentParser a = SegmentParser [Text] (Attoparsec.Parser a)

data Err =
  ClientErr Text |
  ServerErr Text

newtype Realm = Realm ByteString

newtype MediaType = MediaType HttpMedia.MediaType


-- * Execution
-------------------------

serve :: [Resource env ()] -> Word16 -> Bool -> Fx env err Void
serve resourceList port cors =
  Fx.runTotalIO $ \ env -> do
    Warp.run (fromIntegral port) (corsify (buildWaiApplication env () resourceList))
    fail "Warp unexpectedly stopped"
  where
    corsify = if cors then Application.corsify else id

buildWaiApplication :: env -> params -> [Resource env params] -> Wai.Application
buildWaiApplication env params = Application.concat . fmap fromResource where
  fromResource = \ case
    AtResource segment subResourceList ->
      Application.matchSegment segment (buildWaiApplication env params subResourceList)
    ByResource (SegmentParser _ parser) subResourceList ->
      Application.attoparseSegment parser $ \ segment ->
      buildWaiApplication env (segment, params) subResourceList
    AuthenticatedResource (Realm realm) handler subResourceList ->
      Application.authorizing realm $ \ username password request respond ->
      Fx.runFxHandling
        (\ case
          ServerErr err -> do
            Text.hPutStrLn stderr err
            respond Response.internalServerError
          ClientErr err -> respond (Response.plainBadRequest err))
        (Fx.provideAndUse (pure env) (do
            authentication <- handler username password
            case authentication of
              Just identity -> Fx.runTotalIO (const (buildWaiApplication env (identity, params) subResourceList request respond))
              Nothing -> Fx.runTotalIO (const (respond (Response.unauthorized realm)))
          ))
    HandlerResource method receiver responder handler -> \ request -> let
      headers = Wai.requestHeaders request
      (acceptHeader, contentTypeHeader) = headers & Foldl.fold ((,) <$> Foldl.lookup "accept" <*> Foldl.lookup "content-type")
      in if Wai.requestMethod request /= method
        then apply Response.methodNotAllowed
        else case runReceiver receiver contentTypeHeader of
          Nothing -> apply Response.unsupportedMediaType
          Just decoder -> case runResponder responder acceptHeader contentTypeHeader of
            Nothing -> apply Response.notAcceptable
            Just encoder -> \ respond -> do
              requestBody <- Wai.strictRequestBody request
              case decoder (LazyByteString.toStrict requestBody) of
                Left err -> respond (Response.plainBadRequest err)
                Right request ->
                  Fx.runFxHandling
                    (\ case
                      ServerErr err -> do
                        Text.hPutStrLn stderr err
                        respond Response.internalServerError
                      ClientErr err -> respond (Response.plainBadRequest err))
                    (Fx.provideAndUse (pure env) (do
                      response <- handler params request
                      Fx.runTotalIO (const (respond (encoder response)))))
    RedirectResource timeout buildUri -> const $ apply $ case buildUri params of
      Right uri -> Response.temporaryRedirect timeout uri
      Left err -> Response.plainBadRequest err
    _ -> error "TODO"

runReceiver :: Receiver request -> Maybe ByteString -> Maybe (ByteString -> Either Text request)
runReceiver (Receiver spec) = \ case
  Just contentType -> HttpMedia.mapContentMedia mediaAssocList contentType
  Nothing -> case spec of
    (_, refiner) : _ -> Just refiner
    _ -> Nothing
  where
    mediaAssocList =
      spec &
      foldl' (\ map (mediaType, refiner) ->
        Map.insertWith alternateRefiners mediaType refiner map) Map.empty &
      Map.toList
      where
        alternateRefiners l r source = case l source of
          Left err -> r source
          Right res -> Right res

runResponder :: Responder response -> Maybe ByteString -> Maybe ByteString -> Maybe (response -> Wai.Response)
runResponder (Responder spec) acceptHeader contentTypeHeader =
  byAccept <|> byContentType <|> byHead
  where
    byAccept = acceptHeader >>= HttpMedia.mapAcceptMedia spec
    byContentType = contentTypeHeader >>= HttpMedia.mapContentMedia spec
    byHead = case spec of
      (_, a) : _ -> Just a
      _ -> Nothing


-- * DSL
-------------------------

at :: Text -> [Resource env params] -> Resource env params
at = AtResource

by :: SegmentParser segment -> [Resource env (segment, params)] -> Resource env params
by = ByResource

head :: (params -> Fx env Err ()) -> Resource env params
head handler = HandlerResource "head" mempty mempty (\ params () -> handler params)

get :: Responder response -> (params -> Fx env Err response) -> Resource env params
get responder handler = HandlerResource "get" mempty responder (\ params () -> handler params)

post :: Receiver request -> Responder response -> (params -> request -> Fx env Err response) -> Resource env params
post = HandlerResource "post"

put :: Receiver request -> Responder response -> (params -> request -> Fx env Err response) -> Resource env params
put = HandlerResource "put"

delete :: Responder response -> (params -> Fx env Err response) -> Resource env params
delete responder handler = HandlerResource "delete" mempty responder (\ params () -> handler params)

authenticated :: Realm -> (Text -> Text -> Fx env Err (Maybe identity)) -> [Resource env (identity, params)] -> Resource env params
authenticated = AuthenticatedResource

temporaryRedirect :: Int -> (params -> Either Text Text) -> Resource env params
temporaryRedirect = RedirectResource

-- ** SegmentParser
-------------------------

segment :: Text -> Attoparsec.Parser segment -> SegmentParser segment
segment description = SegmentParser [description]

-- ** Receiver
-------------------------

ofJsonAst :: (Aeson.Value -> Either Text request) -> Receiver request
ofJsonAst aesonParser = ofJsonBytes decoder where
  decoder = first fromString . Aeson.eitherDecodeStrict' >=> aesonParser

ofJsonBytes :: (ByteString -> Either Text request) -> Receiver request
ofJsonBytes decoder = Receiver (fmap (,decoder) MimeTypeList.json)

ofYamlAst :: (Aeson.Value -> Either Text request) -> Receiver request
ofYamlAst aesonParser = ofYamlBytes decoder where
  decoder input = do
    ast <- left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
    aesonParser ast

ofYamlBytes :: (ByteString -> Either Text request) -> Receiver request
ofYamlBytes decoder = Receiver (fmap (,decoder) MimeTypeList.yaml)

-- ** Responder
-------------------------

asJson :: Responder Aeson.Value
asJson = Responder (fmap (,responseFn) MimeTypeList.json) where
  responseFn response = Response.ok "application/json" (Aeson.fromEncoding (Aeson.toEncoding response))

asYaml :: Responder Aeson.Value
asYaml = Responder (fmap (,responseFn) MimeTypeList.yaml) where
  responseFn response = Response.ok "application/yaml" (Aeson.fromEncoding (Aeson.toEncoding response))

asFile :: MediaType -> Responder FilePath
asFile (MediaType contentType) = Responder [(contentType, Response.file (HttpMedia.renderHeader contentType))]


-- * Instances
-------------------------

instance Contravariant (Resource env) where
  contramap fn = \ case
    AtResource segment nodeList -> AtResource segment (fmap (contramap fn) nodeList)
    ByResource parser nodeList -> ByResource parser (fmap (contramap (second fn)) nodeList)
    HandlerResource method receiver responder handler -> HandlerResource method receiver responder (handler . fn)
    _ -> error "TODO"

deriving instance Functor SegmentParser

instance Semigroup (SegmentParser a) where
  (<>) (SegmentParser details1 parser1) (SegmentParser details2 parser2) =
    SegmentParser (details1 <> details2) (parser1 <|> parser2)

instance Monoid (SegmentParser a) where
  mempty = SegmentParser [] empty
  mappend = (<>)

deriving instance Functor Receiver

instance Semigroup (Receiver a) where
  (<>) (Receiver spec1) (Receiver spec2) = Receiver (spec1 <> spec2)

instance Monoid (Receiver a) where
  mempty = Receiver []
  mappend = (<>)

instance Contravariant Responder where
  contramap mapper (Responder spec) = Responder (fmap (second (. mapper)) spec)

instance Semigroup (Responder a) where
  (<>) (Responder spec1) (Responder spec2) = Responder (spec1 <> spec2)

instance Monoid (Responder a) where
  mempty = Responder []
  mappend = (<>)

instance IsString Realm where
  fromString string = if all (\ a -> isAscii a && isPrint a && a /= '"') string
    then Realm (fromString string)
    else error "Not a valid realm"

deriving instance Show MediaType
deriving instance IsString MediaType
deriving instance Ord MediaType
deriving instance Eq MediaType
