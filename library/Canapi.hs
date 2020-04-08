module Canapi (
    Resource,
    SegmentParser,
    Receiver,
    Responder,
    Realm,
    MediaType,
    Err(..),
    -- * Execution
    run,
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
    asAny,
    asJson,
    asYaml,
    asFile,
  ) where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi.Data
import qualified Attoparsec.Data as AttoparsecData
import qualified Canapi.Application as Application
import qualified Canapi.HttpStatus as HttpStatus
import qualified Canapi.MimeTypeList as MimeTypeList
import qualified Canapi.Prelude as Prelude
import qualified Canapi.RequestAccessor as RequestAccessor
import qualified Canapi.Response as Response
import qualified Canapi.RoutingTree as RoutingTree
import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified Fx
import qualified Network.HTTP.Media as HttpMedia
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Internal as Wai
import qualified WaiAppStatic.Types as WaiStatic


-- * Types
-------------------------

newtype Resource env params = Resource [ResourceNode env params]

data ResourceNode env params =
  AtResourceNode Text [ResourceNode env params] |
  forall segment. ByResourceNode (SegmentParser segment) [ResourceNode env (segment, params)] |
  forall identity. AuthenticatedResourceNode Realm (Text -> Text -> Fx env Err (Maybe identity)) [ResourceNode env (identity, params)] |
  forall request response. HandlerResourceNode HttpTypes.Method (Receiver request) (Responder response) (params -> request -> Fx env Err response) |
  RedirectResourceNode Int (params -> Either Text Text) |
  FileSystemResourceNode FilePath

data Receiver a =
  TypedReceiver HttpMedia.MediaType (Map HttpMedia.MediaType (Decoder a)) |
  UntypedReceiver (Decoder a)

type Decoder a = ByteString -> Either Text a

data Responder a =
  TypedResponder HttpMedia.MediaType (Map HttpMedia.MediaType (Encoder a)) |
  UntypedResponder (Encoder a) |
  EmptyResponder

type Encoder a = a -> Wai.Response

data SegmentParser a = SegmentParser [Text] (Attoparsec.Parser a)

data Err =
  ClientErr Text |
  ServerErr Text

newtype Realm = Realm ByteString

newtype MediaType = MediaType HttpMedia.MediaType


-- * Execution
-------------------------

run :: Resource env () -> Word16 -> Bool -> Fx env err ()
run resource port cors =
  Fx.runTotalIO $ \ env -> Warp.run (fromIntegral port) (application env)
  where
    application env =
      resourceRoutingTree env () resource &
      Application.routingTree &
      if cors then Application.corsify else id

resourceRoutingTree :: env -> params -> Resource env params -> RoutingTree.RoutingTree
resourceRoutingTree env params (Resource resourceNodeList) = resourceNodeListRoutingTree env params resourceNodeList

resourceNodeListRoutingTree :: env -> params -> [ResourceNode env params] -> RoutingTree.RoutingTree
resourceNodeListRoutingTree env params = foldMap (resourceNodeRoutingTree env params)

resourceNodeRoutingTree :: env -> params -> ResourceNode env params -> RoutingTree.RoutingTree
resourceNodeRoutingTree env params = \ case
  AtResourceNode segment subResourceNodeList ->
    RoutingTree.RoutingTree
      (\ actualSegment -> if segment == actualSegment
        then Right (resourceNodeListRoutingTree env params subResourceNodeList)
        else Left Nothing)
      mempty
  ByResourceNode (SegmentParser _ segmentParser) subResourceNodeList ->
    RoutingTree.RoutingTree
      (\ segment ->
        Attoparsec.parseOnly (segmentParser <* Attoparsec.endOfInput) segment &
        bimap (Just . fromString) (\ segment ->
          resourceNodeListRoutingTree env (segment, params) subResourceNodeList))
      mempty
  HandlerResourceNode method receiver responder handler ->
    RoutingTree.RoutingTree (const (Left Nothing)) map
    where
      map = Map.fromList [(method, routingHandler)]
      routingHandler contentType accept input =
        case runReceiver receiver contentType input of
          Left earlyResponse -> return earlyResponse
          Right request -> case runResponder responder accept contentType of
            Left earlyResponse -> return earlyResponse
            Right encoder ->
              Fx.runFxHandling @IO
                (\ case
                  ServerErr err -> do
                    Text.hPutStrLn stderr err
                    return Response.internalServerError
                  ClientErr err -> return ((Response.status . HttpStatus.badRequest) err))
                (Fx.provideAndUse (pure env) (do
                  response <- handler params request
                  Fx.runTotalIO (const (return (encoder response)))))
  _ -> error "TODO"

runReceiver :: Receiver a -> Maybe ByteString -> ByteString -> Either Wai.Response a
runReceiver = \ case
  TypedReceiver defaultType map -> let
    mediaAssocList = Map.toList map
    defaultDecoder =
      Map.lookup defaultType map &
      fmap (fmap (first (Response.status . HttpStatus.badRequest))) &
      fromMaybe (const (Left ((Response.status . HttpStatus.internalServerError) "No content decoder")))
    in \ case
      Just contentType -> case HttpMedia.mapContentMedia mediaAssocList contentType of
        Just decoder -> first (Response.status . HttpStatus.badRequest) . decoder
        Nothing -> const (Left ((Response.status . HttpStatus.unsupportedMediaType) err)) where
          err =
            "Unsupported content-type. Expecting one of the following: " <>
            fromString (show (fmap fst mediaAssocList))
      Nothing -> defaultDecoder
  UntypedReceiver decoder -> const (first (Response.status . HttpStatus.badRequest) . decoder)

runResponder :: Responder response -> Maybe ByteString -> Maybe ByteString -> Either Wai.Response (response -> Wai.Response)
runResponder = \ case
  TypedResponder defaultType encoderMap -> \ acceptHeader contentTypeHeader -> let
    encoderAssocList = Map.toList encoderMap
    errResponse = (Response.status . HttpStatus.notAcceptable) message where
      message =
        "Not acceptable. Can only produce content of the following types: " <>
        fromString (show (fmap fst encoderAssocList))
    encoderMaybe = case acceptHeader of
      Just acceptHeader -> HttpMedia.mapAcceptMedia encoderAssocList acceptHeader
      Nothing -> byContentType <|> byDefault where
        byContentType = contentTypeHeader >>= HttpMedia.mapContentMedia encoderAssocList
        byDefault = Map.lookup defaultType encoderMap
    in encoderMaybe & maybe (Left errResponse) Right
  UntypedResponder encoder -> const (const (Right encoder))
  EmptyResponder -> const (const (Left (Response.status (HttpStatus.internalServerError "No encoder"))))


-- * DSL
-------------------------

at :: Text -> Resource env params -> Resource env params
at segment (Resource resourceNodeList) = AtResourceNode segment resourceNodeList & pure & Resource

by :: SegmentParser segment -> Resource env (segment, params) -> Resource env params
by segmentParser (Resource resourceNodeList) = ByResourceNode segmentParser resourceNodeList & pure & Resource

head :: (params -> Fx env Err ()) -> Resource env params
head handler = HandlerResourceNode "HEAD" (pure ()) asAny (\ params () -> handler params) & pure & Resource

get :: Responder response -> (params -> Fx env Err response) -> Resource env params
get responder handler = HandlerResourceNode "GET" (pure ()) responder (\ params () -> handler params) & pure & Resource

post :: Receiver request -> Responder response -> (params -> request -> Fx env Err response) -> Resource env params
post receiver responder handler = HandlerResourceNode "POST" receiver responder handler & pure & Resource

put :: Receiver request -> Responder response -> (params -> request -> Fx env Err response) -> Resource env params
put receiver responder handler = HandlerResourceNode "PUT" receiver responder handler & pure & Resource

delete :: Responder response -> (params -> Fx env Err response) -> Resource env params
delete responder handler = HandlerResourceNode "DELETE" (pure ()) responder (\ params () -> handler params) & pure & Resource

authenticated :: Realm -> (Text -> Text -> Fx env Err (Maybe identity)) -> Resource env (identity, params) -> Resource env params
authenticated realm handler (Resource resourceNodeList) = AuthenticatedResourceNode realm handler resourceNodeList & pure & Resource

temporaryRedirect :: Int -> (params -> Either Text Text) -> Resource env params
temporaryRedirect timeout uriBuilder = RedirectResourceNode timeout uriBuilder & pure & Resource

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
ofJsonBytes decoder = TypedReceiver (Prelude.head MimeTypeList.json) (Map.fromList (fmap (,decoder) MimeTypeList.json))

ofYamlAst :: (Aeson.Value -> Either Text request) -> Receiver request
ofYamlAst aesonParser = ofYamlBytes decoder where
  decoder input = do
    ast <- left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
    aesonParser ast

ofYamlBytes :: (ByteString -> Either Text request) -> Receiver request
ofYamlBytes decoder = TypedReceiver (Prelude.head MimeTypeList.yaml) (Map.fromList (fmap (,decoder) MimeTypeList.yaml))

-- ** Responder
-------------------------

asAny :: Responder ()
asAny = UntypedResponder (const (Response.status (HttpStatus.ok "")))

asOkay :: [HttpMedia.MediaType] -> (a -> Data.ByteString.Builder.Builder) -> Responder a
asOkay typeList encoder = TypedResponder defaultType (Map.fromList (fmap (,responseFn) typeList)) where
  responseFn = Response.ok (HttpMedia.renderHeader defaultType) . encoder
  defaultType = Prelude.head typeList

asJson :: Responder Aeson.Value
asJson = asOkay MimeTypeList.json (Aeson.fromEncoding . Aeson.toEncoding)

asYaml :: Responder Aeson.Value
asYaml = asOkay MimeTypeList.yaml (Data.ByteString.Builder.byteString . Yaml.encode)

asFile :: MediaType -> Responder FilePath
asFile (MediaType mediaType) = TypedResponder mediaType (Map.singleton mediaType encoder) where
  encoder = Response.file (HttpMedia.renderHeader mediaType)


-- * Instances
-------------------------

instance Contravariant (Resource env) where
  contramap fn (Resource list) = Resource (fmap (contramap fn) list)

deriving instance Semigroup (Resource env params)

deriving instance Monoid (Resource env params)

instance IsList (Resource env params) where
  type Item (Resource env params) = Resource env params
  fromList = mconcat
  toList (Resource list) = fmap (Resource . pure) list

instance Contravariant (ResourceNode env) where
  contramap fn = \ case
    AtResourceNode segment nodeList -> AtResourceNode segment (fmap (contramap fn) nodeList)
    ByResourceNode parser nodeList -> ByResourceNode parser (fmap (contramap (second fn)) nodeList)
    HandlerResourceNode method receiver responder handler -> HandlerResourceNode method receiver responder (handler . fn)
    _ -> error "TODO"

deriving instance Functor SegmentParser

instance Semigroup (SegmentParser a) where
  (<>) (SegmentParser details1 parser1) (SegmentParser details2 parser2) =
    SegmentParser (details1 <> details2) (parser1 <|> parser2)

instance Monoid (SegmentParser a) where
  mempty = SegmentParser [] empty
  mappend = (<>)

deriving instance Functor Receiver

instance Applicative Receiver where
  pure a = UntypedReceiver (const (Right a))
  (<*>) = \ case
    UntypedReceiver dec1 -> \ case
      UntypedReceiver dec2 -> UntypedReceiver (liftA2 (<*>) dec1 dec2)
      TypedReceiver defType2 map2 -> TypedReceiver defType2 (fmap (liftA2 (<*>) dec1) map2)
    TypedReceiver defType1 map1 -> \ case
      UntypedReceiver dec2 -> TypedReceiver defType1 (fmap (\ dec1 -> liftA2 (<*>) dec1 dec2) map1)
      TypedReceiver defType2 map2 -> TypedReceiver defType1 (Map.intersectionWith intersect map1 map2) where
        intersect = liftA2 (<*>)

instance Alternative Receiver where
  empty = UntypedReceiver (const (Left "No decoder"))
  (<|>) = \ case
    UntypedReceiver dec1 -> const (UntypedReceiver dec1)
    TypedReceiver defType1 map1 -> \ case
      UntypedReceiver dec2 -> UntypedReceiver dec2
      TypedReceiver defType2 map2 -> TypedReceiver defType1 (Map.unionWith union map1 map2) where
        union = liftA2 (<!>)

instance Contravariant Responder where
  contramap mapper = \ case
    TypedResponder defaultType encoderMap -> TypedResponder defaultType (fmap (lmap mapper) encoderMap)
    UntypedResponder encoder -> UntypedResponder (encoder . mapper)
    EmptyResponder -> EmptyResponder

instance Semigroup (Responder a) where
  (<>) = \ case
    UntypedResponder enc1 -> const (UntypedResponder enc1)
    TypedResponder defType1 map1 -> \ case
      UntypedResponder enc2 -> UntypedResponder enc2
      TypedResponder defType2 map2 -> TypedResponder defType1 (Map.union map1 map2)
      EmptyResponder -> TypedResponder defType1 map1
    EmptyResponder -> id

instance Monoid (Responder a) where
  mempty = EmptyResponder
  mappend = (<>)

instance IsString Realm where
  fromString string = if all (\ a -> isAscii a && isPrint a && a /= '"') string
    then Realm (fromString string)
    else error "Not a valid realm"

deriving instance Show MediaType
deriving instance IsString MediaType
deriving instance Ord MediaType
deriving instance Eq MediaType
