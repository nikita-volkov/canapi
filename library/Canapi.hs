module Canapi
  ( -- * Types
    Resource,
    SegmentParser,
    Receiver,
    Renderer,
    Realm,
    MediaType,
    Err (..),

    -- * Execution
    run,

    -- * Resource

    -- ** Segments
    at,
    by,

    -- ** Methods
    head,
    get,
    post,
    put,
    delete,

    -- ** Authentication
    authenticated,

    -- ** Redirection
    temporaryRedirect,

    -- * SegmentParser
    segment,

    -- * Receiver
    ofJsonAst,
    ofJsonBytes,
    ofYamlAst,
    ofYamlBytes,
    ofBinary,

    -- * Renderer
    asAny,
    asJson,
    asYaml,
    asHtml,
    asXhtml,
    asFile,
    asBinary,
  )
where

import qualified Canapi.Application as Application
import qualified Canapi.HttpStatus as HttpStatus
import qualified Canapi.MimeTypeList as MimeTypeList
import Canapi.Prelude hiding (delete, head)
import qualified Canapi.Prelude as Prelude
import qualified Canapi.Response as Response
import qualified Canapi.RoutingTree as RoutingTree
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as BsAtto
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString.Builder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as Cereal
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Media as HttpMedia
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- * Types

-------------------------

data ResourceNode params
  = AtResourceNode Text [ResourceNode params]
  | forall segment. ByResourceNode (SegmentParser segment) [ResourceNode (segment, params)]
  | forall identity. AuthenticatedResourceNode Realm (Text -> Text -> IO (Either Err (Maybe identity))) [ResourceNode (identity, params)]
  | forall request response. HandlerResourceNode HttpTypes.Method (Receiver request) (Renderer response) (request -> params -> IO (Either Err response))
  | RedirectResourceNode Int (params -> Either Text Text)
  | FileSystemResourceNode FilePath

-- |
-- Alternation is about lookup.
-- The value parser failing gets propagated to the top.
newtype CookiesParser cookies
  = CookiesParser (HashMap ByteString ByteString -> Either Text (Maybe cookies))
  deriving
    (Functor, Applicative, Alternative)
    via (ReaderT (HashMap ByteString ByteString) (MaybeT (Either Text)))

data Receiver a
  = TypedReceiver
      HttpMedia.MediaType
      -- ^ Default type. Used when no content-type is provided.
      (Map HttpMedia.MediaType (Decoder a))
      -- ^ Map of decoders based on content-type.
  | UntypedReceiver (Decoder a)

type Decoder a = ByteString -> Either Text a

-- |
-- Renderer of response.
--
-- Automates the choice of different renderers depending on the requested type.
-- When the requested type is not supported, downgrades to the Canapi default handler.
--
-- Therefore it is advised to have your own default handler appended to all your renderers.
data Renderer a
  = TypedRenderer HttpMedia.MediaType (Map HttpMedia.MediaType (Encoder a))
  | UntypedRenderer (Encoder a)
  | EmptyRenderer

type Encoder a = a -> Wai.Response

data SegmentParser a = SegmentParser [Text] (Attoparsec.Parser a)

data Err
  = ClientErr Text
  | ServerErr Text

newtype Realm = Realm ByteString

newtype MediaType = MediaType HttpMedia.MediaType

-- * Execution

-------------------------

run :: [ResourceNode ()] -> Word16 -> Bool -> IO ()
run resource port cors =
  Warp.run (fromIntegral port) application
  where
    application =
      resourceNodeListRoutingTree () resource
        & Application.routingTree
        & if cors then Application.corsify else id

resourceNodeListRoutingTree :: params -> [ResourceNode params] -> RoutingTree.RoutingTree
resourceNodeListRoutingTree params = foldMap (resourceNodeRoutingTree params)

resourceNodeRoutingTree :: params -> ResourceNode params -> RoutingTree.RoutingTree
resourceNodeRoutingTree params = \case
  AtResourceNode segment subResourceNodeList ->
    RoutingTree.RoutingTree
      ( \actualSegment ->
          if segment == actualSegment
            then Right (resourceNodeListRoutingTree params subResourceNodeList)
            else Left Nothing
      )
      mempty
  ByResourceNode (SegmentParser _ segmentParser) subResourceNodeList ->
    RoutingTree.RoutingTree
      ( \segment ->
          Attoparsec.parseOnly (segmentParser <* Attoparsec.endOfInput) segment
            & bimap
              (Just . fromString)
              ( \segment ->
                  resourceNodeListRoutingTree (segment, params) subResourceNodeList
              )
      )
      mempty
  HandlerResourceNode method receiver renderer handler ->
    RoutingTree.RoutingTree (const (Left Nothing)) map
    where
      map = Map.fromList [(method, routingHandler)]
      routingHandler contentType accept input =
        case runReceiver receiver contentType input of
          Left earlyResponse -> return earlyResponse
          Right request -> case runRenderer renderer accept contentType of
            Left earlyResponse -> return earlyResponse
            Right encoder -> do
              response <- handler request params
              case response of
                Left err -> case err of
                  ServerErr err -> do
                    Text.hPutStrLn stderr err
                    return Response.internalServerError
                  ClientErr err -> return $ Response.status $ HttpStatus.badRequest $ err
                Right res -> return $ encoder res
  _ -> error "TODO"

runReceiver :: Receiver a -> Maybe ByteString -> ByteString -> Either Wai.Response a
runReceiver = \case
  TypedReceiver defaultType map ->
    let mediaAssocList = Map.toList map
        defaultDecoder =
          Map.lookup defaultType map
            & fmap (fmap (first (Response.status . HttpStatus.badRequest)))
            & fromMaybe (const (Left ((Response.status . HttpStatus.internalServerError) "No content decoder")))
     in \case
          Just contentType -> case HttpMedia.mapContentMedia mediaAssocList contentType of
            Just decoder -> first (Response.status . HttpStatus.badRequest) . decoder
            Nothing -> const (Left ((Response.status . HttpStatus.unsupportedMediaType) err))
              where
                err =
                  "Unsupported content-type. Expecting one of the following: "
                    <> fromString (show (fmap fst mediaAssocList))
          Nothing -> defaultDecoder
  UntypedReceiver decoder -> const (first (Response.status . HttpStatus.badRequest) . decoder)

runRenderer :: Renderer response -> Maybe ByteString -> Maybe ByteString -> Either Wai.Response (response -> Wai.Response)
runRenderer = \case
  TypedRenderer defaultType encoderMap -> \acceptHeader contentTypeHeader ->
    let encoderAssocList = Map.toList encoderMap
        errResponse = (Response.status . HttpStatus.notAcceptable) message
          where
            message =
              "Not acceptable. Can only produce content of the following types: "
                <> fromString (show (fmap fst encoderAssocList))
        encoderMaybe = case acceptHeader of
          Just acceptHeader -> HttpMedia.mapAcceptMedia encoderAssocList acceptHeader
          Nothing -> byContentType <|> byDefault
            where
              byContentType = contentTypeHeader >>= HttpMedia.mapContentMedia encoderAssocList
              byDefault = Map.lookup defaultType encoderMap
     in encoderMaybe & maybe (Left errResponse) Right
  UntypedRenderer encoder -> const (const (Right encoder))
  EmptyRenderer -> const (const (Left (Response.status (HttpStatus.internalServerError "No encoder"))))

-- * DSL

-------------------------

withCookies :: CookiesParser cookies -> [ResourceNode (cookies, params)] -> [ResourceNode params]
withCookies =
  error "TODO"

at :: Text -> [ResourceNode params] -> [ResourceNode params]
at segment resourceNodeList = AtResourceNode segment resourceNodeList & pure

by :: SegmentParser segment -> [ResourceNode (segment, params)] -> [ResourceNode params]
by segmentParser resourceNodeList = ByResourceNode segmentParser resourceNodeList & pure

head :: (params -> IO (Either Err ())) -> [ResourceNode params]
head handler = HandlerResourceNode "HEAD" (pure ()) asAny (\() params -> handler params) & pure

get :: Renderer response -> (params -> IO (Either Err response)) -> [ResourceNode params]
get renderer handler = HandlerResourceNode "GET" (pure ()) renderer (\() params -> handler params) & pure

post :: Receiver request -> Renderer response -> (request -> params -> IO (Either Err response)) -> [ResourceNode params]
post receiver renderer handler = HandlerResourceNode "POST" receiver renderer handler & pure

put :: Receiver request -> Renderer response -> (request -> params -> IO (Either Err response)) -> [ResourceNode params]
put receiver renderer handler = HandlerResourceNode "PUT" receiver renderer handler & pure

delete :: Renderer response -> (params -> IO (Either Err response)) -> [ResourceNode params]
delete renderer handler = HandlerResourceNode "DELETE" (pure ()) renderer (\() params -> handler params) & pure

authenticated :: Realm -> (Text -> Text -> IO (Either Err (Maybe identity))) -> [ResourceNode (identity, params)] -> [ResourceNode params]
authenticated realm handler resourceNodeList = AuthenticatedResourceNode realm handler resourceNodeList & pure

temporaryRedirect :: Int -> (params -> Either Text Text) -> [ResourceNode params]
temporaryRedirect timeout uriBuilder = RedirectResourceNode timeout uriBuilder & pure

-- ** SegmentParser

-------------------------

segment :: Text -> Attoparsec.Parser segment -> SegmentParser segment
segment description = SegmentParser [description]

-- ** Receiver

-------------------------

unitTypedReceiver typeList decoder =
  TypedReceiver
    (Prelude.head typeList)
    (Map.fromList (fmap (,decoder) typeList))

ofJsonAst :: (Aeson.Value -> Either Text request) -> Receiver request
ofJsonAst aesonParser = ofJsonBytes decoder
  where
    decoder = first fromString . Aeson.eitherDecodeStrict' >=> aesonParser

ofJsonBytes :: (ByteString -> Either Text request) -> Receiver request
ofJsonBytes = unitTypedReceiver MimeTypeList.json

ofYamlAst :: (Aeson.Value -> Either Text request) -> Receiver request
ofYamlAst aesonParser = ofYamlBytes decoder
  where
    decoder input = do
      ast <- left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)
      aesonParser ast

ofYamlBytes :: (ByteString -> Either Text request) -> Receiver request
ofYamlBytes = unitTypedReceiver MimeTypeList.yaml

ofBinary :: Cereal.Get request -> Receiver request
ofBinary = unitTypedReceiver MimeTypeList.binary . cerealBinaryDecoder

-- ** Decoder

-------------------------

cerealBinaryDecoder :: Cereal.Get a -> Decoder a
cerealBinaryDecoder = fmap (first fromString) . Cereal.runGet

-- ** Renderer

-------------------------

asAny :: Renderer ()
asAny = UntypedRenderer (const (Response.status (HttpStatus.ok "")))

asOkay :: [HttpMedia.MediaType] -> (a -> Data.ByteString.Builder.Builder) -> Renderer a
asOkay typeList encoder = TypedRenderer defaultType (Map.fromList (fmap (,responseFn) typeList))
  where
    responseFn = Response.ok (HttpMedia.renderHeader defaultType) . encoder
    defaultType = Prelude.head typeList

asJson :: Renderer Aeson.Value
asJson = asOkay MimeTypeList.json (Aeson.fromEncoding . Aeson.toEncoding)

asYaml :: Renderer Aeson.Value
asYaml = asOkay MimeTypeList.yaml (Data.ByteString.Builder.byteString . Yaml.encode)

asHtml :: (a -> Data.ByteString.Builder.Builder) -> Renderer a
asHtml = asOkay MimeTypeList.html

asXhtml :: (a -> Data.ByteString.Builder.Builder) -> Renderer a
asXhtml = asOkay MimeTypeList.xhtml

asFile :: MediaType -> Renderer FilePath
asFile (MediaType mediaType) = TypedRenderer mediaType (Map.singleton mediaType encoder)
  where
    encoder = Response.file (HttpMedia.renderHeader mediaType)

asBinary :: Cereal.Putter a -> Renderer a
asBinary putter =
  asOkay MimeTypeList.binary $ Cereal.execPut . putter

-- * CookiesParser

-------------------------

cookieByName :: ByteString -> BsAtto.Parser a -> CookiesParser a
cookieByName name parser =
  CookiesParser $ \map -> case HashMap.lookup name map of
    Just cookieBytes -> BsAtto.parseOnly parser cookieBytes & bimap fromString Just
    Nothing -> Right Nothing

-- * Instances

-------------------------

instance Contravariant ResourceNode where
  contramap fn = \case
    AtResourceNode segment nodeList -> AtResourceNode segment (fmap (contramap fn) nodeList)
    ByResourceNode parser nodeList -> ByResourceNode parser (fmap (contramap (second fn)) nodeList)
    HandlerResourceNode method receiver renderer handler -> HandlerResourceNode method receiver renderer (\request params -> handler request (fn params))
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
  (<*>) = \case
    UntypedReceiver dec1 -> \case
      UntypedReceiver dec2 -> UntypedReceiver (liftA2 (<*>) dec1 dec2)
      TypedReceiver defType2 map2 -> TypedReceiver defType2 (fmap (liftA2 (<*>) dec1) map2)
    TypedReceiver defType1 map1 -> \case
      UntypedReceiver dec2 -> TypedReceiver defType1 (fmap (\dec1 -> liftA2 (<*>) dec1 dec2) map1)
      TypedReceiver _ map2 -> TypedReceiver defType1 (Map.intersectionWith intersect map1 map2)
        where
          intersect = liftA2 (<*>)

instance Alternative Receiver where
  empty = UntypedReceiver (const (Left "No decoder"))
  (<|>) = \case
    UntypedReceiver dec1 -> const (UntypedReceiver dec1)
    TypedReceiver defType1 map1 -> \case
      UntypedReceiver dec2 -> UntypedReceiver dec2
      TypedReceiver _ map2 -> TypedReceiver defType1 (Map.unionWith union map1 map2)
        where
          union l r i = case l i of
            Left _ -> r i
            a -> a

instance Contravariant Renderer where
  contramap mapper = \case
    TypedRenderer defaultType encoderMap -> TypedRenderer defaultType (fmap (lmap mapper) encoderMap)
    UntypedRenderer encoder -> UntypedRenderer (encoder . mapper)
    EmptyRenderer -> EmptyRenderer

instance Semigroup (Renderer a) where
  (<>) = \case
    UntypedRenderer enc1 -> const (UntypedRenderer enc1)
    TypedRenderer defType1 map1 -> \case
      UntypedRenderer enc2 -> UntypedRenderer enc2
      TypedRenderer _ map2 -> TypedRenderer defType1 (Map.union map1 map2)
      EmptyRenderer -> TypedRenderer defType1 map1
    EmptyRenderer -> id

instance Monoid (Renderer a) where
  mempty = EmptyRenderer
  mappend = (<>)

instance IsString Realm where
  fromString string =
    if all (\a -> isAscii a && isPrint a && a /= '"') string
      then Realm (fromString string)
      else error "Not a valid realm"

deriving instance Show MediaType

deriving instance IsString MediaType

deriving instance Ord MediaType

deriving instance Eq MediaType
