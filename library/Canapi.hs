module Canapi (
    -- * IO
    serve,
    -- * Resource
    Resource,
    atSegment,
    binary,
    directory,
    indexFile,
    -- ** Low-level
    waiApplication,
    -- * Metadata
    ClientInfo(..),
  ) where

import Canapi.Prelude
import qualified Canapi.Wai.Response as Response
import qualified Canapi.NetworkIp as NetworkIp
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified WaiAppStatic.Types as WaiStatic
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap


-- * IO
-------------------------

serve :: Resource -> Word16 -> Bool -> IO Void
serve (Resource app) port cors = do
  Warp.run (fromIntegral port) (if cors then corsify app else app)
  fail "The server stopped"
  where
    corsify = WaiCors.cors (const (Just policy)) where
      policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = WaiCors.simpleHeaders }


-- * Resource
-------------------------

newtype Resource = Resource (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived)

instance Semigroup Resource where
  (<>) (Resource a) (Resource b) = Resource $ \ request respond ->
    a request $ \ response1 -> do
      let statusCode1 = HttpTypes.statusCode (Wai.responseStatus response1)
      if statusCode1 >= 400 && statusCode1 < 500
        then b request respond
        else respond response1

instance Monoid Resource where
  mempty = Resource (\ _ respond -> respond Response.notFound)
  mappend = (<>)
  mconcat = \ case
    a : [] -> a
    a : b -> a <> mconcat b
    [] -> mempty

atSegment :: Text -> Resource -> Resource
atSegment expectedSegment (Resource nested) = Resource $ \ request respond ->
  case Wai.pathInfo request of
    segmentsHead : segmentsTail -> if expectedSegment == segmentsHead
      then nested (request { Wai.pathInfo = segmentsTail }) respond
      else respond Response.notFound
    _ -> respond Response.notFound

bySegment :: (Text -> Resource) -> Resource
bySegment = error "TODO"

parsingSegment :: Attoparsec.Parser segment -> (segment -> Resource) -> Resource
parsingSegment = error "TODO"

authenticating :: (Text -> Text -> IO (Maybe identity)) -> (identity -> Resource) -> Resource
authenticating = error "TODO"

{-|
Binary protocol resource.

Only supports the @POST@ method.
-}
binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (ClientInfo -> request -> IO response) ->
  Resource
binary decoder encoder fx = Resource $ \ request respond ->
  if Wai.requestMethod request == HttpTypes.methodPost
    then do
      requestBody <- Wai.strictRequestBody request
      case CerealGet.runGetLazy decoder requestBody of
        Right decodedRequest -> do
          response <- let
            clientInfo = let
              sockAddr = Wai.remoteHost request
              ip = case NetworkIp.sockAddrIP sockAddr of
                Just a -> a
                Nothing -> error (
                    "Warp has set an unexpected remoteHost address: " <> show sockAddr <> ". " <>
                    "Please report this to the maintainers of the \"canapi\" package."
                  )
              userAgent = fmap Text.decodeLatin1 (Wai.requestHeaderUserAgent request)
              referer = fmap Text.decodeLatin1 (Wai.requestHeaderReferer request)
              in ClientInfo ip userAgent referer
            in fx clientInfo decodedRequest
          let
            waiResponse =
              Wai.responseBuilder
                HttpTypes.status200
                []
                (CerealPut.execPut (encoder response))
            in respond waiResponse
        Left err -> respond (Wai.responseLBS HttpTypes.status400 [] (fromString err))
    else respond (Wai.responseLBS HttpTypes.status405 [] "")

directory :: FilePath -> Resource
directory path = let
  settings =
    (WaiStatic.defaultWebAppSettings path) {
        WaiStatic.ssMaxAge = WaiStatic.NoMaxAge
      }
  in waiApplication (WaiStatic.staticApp settings)

indexFile :: Text -> FilePath -> Resource
indexFile contentType path = Resource $ \ request respond ->
  case Wai.pathInfo request of
    [] -> respond $ Wai.responseFile HttpTypes.status200 [("Content-Type", Text.encodeUtf8 contentType)] path Nothing
    _ -> respond $ Response.notFound

post :: ContentDecoder request -> ContentEncoder response -> (request -> IO response) -> Resource
post = error "TODO"

get :: ContentEncoder response -> IO response -> Resource
get = error "TODO"

temporaryRedirect :: Int -> Text -> Resource
temporaryRedirect timeout uri = Resource $ \ _ respond -> respond response where
  response = Wai.responseBuilder HttpTypes.status307 headers "" where
    headers = [cacheControl, location] where
      cacheControl = ("cache-control", "max-age=" <> fromString (show timeout))
      location = ("location", Text.encodeUtf8 uri)


-- ** Low-level
-------------------------

waiApplication :: Wai.Application -> Resource
waiApplication = Resource


-- * Metadata
-------------------------

data ClientInfo = ClientInfo {
    _ip :: IP,
    _userAgent :: Maybe Text,
    _referer :: Maybe Text
  }


-- * Decoder
-------------------------

{-| Request content parser. -}
newtype ContentDecoder request = ContentDecoder (HashMap Text (ByteString -> Either Text request))

instance Semigroup (ContentDecoder request) where
  (<>) (ContentDecoder a) (ContentDecoder b) = ContentDecoder (HashMap.unionWith unite a b) where
    unite a b input = a input & either (const (b input)) Right

instance Monoid (ContentDecoder request) where
  mempty = ContentDecoder HashMap.empty
  mappend = (<>)

deriving instance Functor ContentDecoder

ofJson :: (Aeson.Value -> Either Text request) -> ContentDecoder request
ofJson = error "TODO"

ofBinary :: CerealGet.Get request -> ContentDecoder request
ofBinary = error "TODO"


-- * Encoder
-------------------------

{-| Response content renderer. -}
newtype ContentEncoder response = ContentEncoder (HashMap Text (response -> ByteString))

instance Semigroup (ContentEncoder response) where
  (<>) (ContentEncoder a) (ContentEncoder b) = ContentEncoder (HashMap.union a b)

instance Monoid (ContentEncoder response) where
  mempty = ContentEncoder HashMap.empty
  mappend = (<>)

instance Contravariant ContentEncoder where
  contramap fn (ContentEncoder map) = ContentEncoder (fmap (. fn) map)

asJson :: ContentEncoder Aeson.Value
asJson = error "TODO"

asBinary :: ContentEncoder CerealPut.Put
asBinary = error "TODO"

asFile :: Text -> ContentEncoder FilePath
asFile contentType = error "TODO"
