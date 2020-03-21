module Canapi (
    -- * IO
    serve,
    -- * Resource
    Resource,
    atSegment,
    bySegment,
    parsingSegment,
    get,
    post,
    put,
    delete,
    temporaryRedirect,
    directory,
    waiApplication,
    -- * Receiver
    Receiver,
    ofJson,
    ofYaml,
    -- * Responder
    Responder,
    asJson,
    asYaml,
  ) where

import Canapi.Prelude hiding (delete)
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


-- * IO
-------------------------

serve :: Resource -> Word16 -> Bool -> IO Void
serve resource port cors = do
  Warp.run (fromIntegral port) (if cors then corsify app else app)
  fail "The server stopped"
  where
    app = toWaiApplication resource
    corsify = WaiCors.cors (const (Just policy)) where
      policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = WaiCors.simpleHeaders }

toWaiApplication :: Resource -> Wai.Application
toWaiApplication (Resource resource) request =
  resource (RequestAccessor.requestMetadata request) request


-- * Resource
-------------------------

newtype Resource = Resource (RequestMetadata -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived)

instance Semigroup Resource where
  (<>) (Resource a) (Resource b) = Resource $ \ requestMetadata request respond ->
    a requestMetadata request $ \ response1 -> do
      let statusCode1 = HttpTypes.statusCode (Wai.responseStatus response1)
      if statusCode1 >= 400 && statusCode1 < 500
        then b requestMetadata request respond
        else respond response1

instance Monoid Resource where
  mempty = Resource (\ _ _ respond -> respond Response.notFound)
  mappend = (<>)
  mconcat = \ case
    a : [] -> a
    a : b -> a <> mconcat b
    [] -> mempty

atSegment :: Text -> Resource -> Resource
atSegment expectedSegment (Resource nested) = Resource $ \ requestMetadata request respond ->
  case Wai.pathInfo request of
    segmentsHead : segmentsTail -> if expectedSegment == segmentsHead
      then nested requestMetadata (request { Wai.pathInfo = segmentsTail }) respond
      else respond Response.notFound
    _ -> respond Response.notFound

bySegment :: AttoparsecData.LenientParser segment => (segment -> Resource) -> Resource
bySegment = parsingSegment AttoparsecData.lenientParser

parsingSegment :: Attoparsec.Parser segment -> (segment -> Resource) -> Resource
parsingSegment parser cont = Resource $ \ requestMetadata request respond ->
  case Wai.pathInfo request of
    segmentsHead : segmentsTail ->
      case Attoparsec.parseOnly (parser <* Attoparsec.endOfInput) segmentsHead of
        Right segment -> case cont segment of
          Resource nested -> nested requestMetadata (request { Wai.pathInfo = segmentsTail }) respond
        Left err -> respond (Response.plainBadRequest (fromString err))
    _ -> respond Response.notFound

authenticating :: (Text -> Text -> IO (Maybe identity)) -> (identity -> Resource) -> Resource
authenticating = error "TODO"

get :: Responder response -> IO response -> Resource
get responder handler = endpoint HttpTypes.methodGet (pure ()) responder (const handler)

post :: Receiver request -> Responder response -> (request -> IO response) -> Resource
post = endpoint HttpTypes.methodPost

{-|
Post without producing a result or waiting for it.

Immediately produces a response with the 202 status code.

https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/202
-}
postAsync :: Receiver request -> (request -> IO ()) -> Resource
postAsync = error "TODO"

put :: Receiver request -> Responder response -> (request -> IO response) -> Resource
put = endpoint HttpTypes.methodPut

delete :: Receiver request -> Responder response -> (request -> IO response) -> Resource
delete = endpoint HttpTypes.methodDelete

endpoint :: HttpTypes.Method -> Receiver request -> Responder response -> (request -> IO response) -> Resource
endpoint method receiver responder handler = Resource $ \ requestMetadata request respond ->
  if Wai.requestMethod request /= method
    then respond Response.methodNotAllowed
    else case runReceiver receiver (getField @"contentType" requestMetadata) of
      Nothing -> respond Response.unsupportedMediaType
      Just decoder -> case runResponder responder (getField @"acceptHeader" requestMetadata) (getField @"contentTypeHeader" requestMetadata) of
        Nothing -> respond Response.notAcceptable
        Just encoder -> do
          requestBody <- Wai.strictRequestBody request
          case decoder (LazyByteString.toStrict requestBody) of
            Left err -> respond (Response.plainBadRequest err)
            Right request -> do
              result <- try @SomeException (handler request)
              case result of
                Right result -> respond (encoder result)
                Left exc -> do
                  hPutStrLn stderr (show exc)
                  respond Response.internalServerError

temporaryRedirect :: Int -> Text -> Resource
temporaryRedirect timeout uri = Resource $ \ _ _ respond ->
  respond (Response.temporaryRedirect timeout uri)

directory :: FilePath -> Resource
directory path = let
  settings =
    (WaiStatic.defaultWebAppSettings path) {
        WaiStatic.ssMaxAge = WaiStatic.NoMaxAge
      }
  in waiApplication (WaiStatic.staticApp settings)


-- ** Low-level
-------------------------

waiApplication :: Wai.Application -> Resource
waiApplication app = Resource (\ _ -> app)


-- * Receiver
-------------------------

{-| Request content parser. -}
data Receiver request = Receiver (Maybe (Decoder request)) (ByType.ByType (Maybe (Decoder request)))

type Decoder a = ByteString -> Either Text a

instance Semigroup (Receiver request) where
  (<>) (Receiver a b) (Receiver c d) = Receiver (a <|> c) (liftA2 (liftA2 unite) b d) where
    unite a b input = a input & either (const (b input)) Right

instance Monoid (Receiver request) where
  mempty = Receiver Nothing (pure mempty)
  mappend = (<>)

deriving instance Functor Receiver

instance Applicative Receiver where
  pure a = Receiver (Just (const (Right a))) (pure mempty)
  (<*>) (Receiver a b) (Receiver c d) = Receiver e f where
    e = (liftA2 . liftA2) (<*>) a c
    f = (liftA2 . liftA2 . liftA2) (<*>) b d

runReceiver :: Receiver request -> Maybe Type -> Maybe (ByteString -> Either Text request)
runReceiver (Receiver defaultDecoder decoderByType) contentType = case contentType of
  Just contentType -> ByType.get contentType decoderByType
  Nothing -> defaultDecoder

ofJson :: (Aeson.Value -> Either Text request) -> Receiver request
ofJson aesonParser = Receiver (Just decoder) (ByType.justJson decoder) where
  decoder = first fromString . Aeson.eitherDecodeStrict' >=> aesonParser

ofYaml :: (Aeson.Value -> Either Text request) -> Receiver request
ofYaml aesonParser = Receiver (Just decoder) (ByType.justYaml decoder) where
  decoder input = do
    ast <- left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
    aesonParser ast

ofBinary :: CerealGet.Get request -> Receiver request
ofBinary = error "TODO"


-- * Responder
-------------------------

{-| Response content renderer. -}
newtype Responder response = Responder ([(HttpMedia.MediaType, response -> Wai.Response)])

instance Semigroup (Responder response) where
  (<>) (Responder a) (Responder b) = Responder (a <> b)

instance Monoid (Responder response) where
  mempty = Responder mempty
  mappend = (<>)

instance Contravariant Responder where
  contramap fn (Responder map) = Responder (fmap (second (. fn)) map)

runResponder :: Responder response -> Maybe ByteString -> Maybe ByteString -> Maybe (response -> Wai.Response)
runResponder (Responder spec) acceptHeader contentTypeHeader =
  byAccept <|> byContentType <|> byHead
  where
    byAccept = acceptHeader >>= HttpMedia.mapAcceptMedia spec
    byContentType = contentTypeHeader >>= HttpMedia.mapContentMedia spec
    byHead = case spec of
      (_, a) : _ -> Just a
      _ -> Nothing

asJson :: Responder Aeson.Value
asJson = Responder (MimeTypeList.json & fmap fromString & fmap (,responseFn)) where
  responseFn response = Response.ok "application/json" (Aeson.fromEncoding (Aeson.toEncoding response))

asYaml :: Responder Aeson.Value
asYaml = Responder (MimeTypeList.yaml & fmap fromString & fmap (,responseFn)) where
  responseFn response = Response.ok "application/yaml" (Aeson.fromEncoding (Aeson.toEncoding response))

asBinary :: Responder CerealPut.Put
asBinary = error "TODO"

asFile :: Text -> Responder FilePath
asFile contentType = error "TODO"
