module Canapi (
    -- * IO
    serve,
    serveParsingCliArgs,
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
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Canapi.Wai.Response as Response
import qualified Canapi.NetworkIp as NetworkIp
import qualified Optima
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

serve :: Resource env -> Word16 -> Bool -> Provider Text env -> IO Void
serve (Resource parser) port cors envProvider =
  runFx $ handleErr (fail . Text.unpack) $ provideAndUse envProvider $ handleEnv $ \ env ->
  runTotalIO $ do
    Warp.run (fromIntegral port) $ (if cors then corsApp else id) $ \ request cont ->
      runFx $ handleErr (fail . Text.unpack) $ provideAndUse (pure env) $ parser request cont
    fail "The server stopped"
  where
    corsApp = WaiCors.cors (const (Just policy))
      where
        policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = WaiCors.simpleHeaders }

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Resource env -> Text -> Optima.ParamGroup envArgs -> (envArgs -> Provider Text env) -> IO Void
serveParsingCliArgs api appDesc envParamGroup provider = do
  (port, cors, envArgs) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve api port cors (provider envArgs)


-- * Resource
-------------------------

newtype Resource env = Resource (Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> Fx env Text Wai.ResponseReceived)

instance Semigroup (Resource env) where
  (<>) (Resource a) (Resource b) = Resource $ \ request respond -> handleEnv $ \ env ->
    a request $ \ response1 -> do
      let statusCode1 = HttpTypes.statusCode (Wai.responseStatus response1)
      if statusCode1 >= 400 && statusCode1 < 500
        then runFx $ handleErr (fail . Text.unpack) $ provideAndUse (pure env) $ b request respond
        else respond response1

instance Monoid (Resource env) where
  mempty = Resource (\ _ respond -> runTotalIO (respond Response.notFound))
  mappend = (<>)
  mconcat = \ case
    a : [] -> a
    a : b -> a <> mconcat b
    [] -> mempty

instance Contravariant Resource where
  contramap f (Resource a) = Resource (\ request respond -> mapEnv f (a request respond))

atSegment :: Text -> Resource env -> Resource env
atSegment expectedSegment (Resource nested) = Resource $ \ request respond ->
  case Wai.pathInfo request of
    segmentsHead : segmentsTail -> if expectedSegment == segmentsHead
      then nested (request { Wai.pathInfo = segmentsTail }) respond
      else runTotalIO (respond Response.notFound)
    _ -> runTotalIO (respond Response.notFound)

bySegment :: (Text -> Resource env) -> Resource env
bySegment = error "TODO"

parsingSegment :: Attoparsec.Parser segment -> (segment -> Resource env) -> Resource env
parsingSegment = error "TODO"

authenticating :: (Text -> Text -> Fx env Text (Maybe identity)) -> (identity -> Resource env) -> Resource env
authenticating = error "TODO"

{-|
Binary protocol resource.

Only supports the @POST@ method.
-}
binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (ClientInfo -> request -> Fx env Text response) ->
  Resource env
binary decoder encoder fx = Resource $ \ request respond ->
  if Wai.requestMethod request == HttpTypes.methodPost
    then do
      requestBody <- runTotalIO $ Wai.strictRequestBody request
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
            in runTotalIO (respond waiResponse)
        Left err -> runTotalIO (respond (Wai.responseLBS HttpTypes.status400 [] (fromString err)))
    else runTotalIO (respond (Wai.responseLBS HttpTypes.status405 [] ""))

directory :: FilePath -> Resource env
directory path = let
  settings =
    (WaiStatic.defaultWebAppSettings path) {
        WaiStatic.ssMaxAge = WaiStatic.NoMaxAge
      }
  in waiApplication (WaiStatic.staticApp settings)

indexFile :: Text -> FilePath -> Resource env
indexFile contentType path = Resource $ \ request respond ->
  case Wai.pathInfo request of
    [] -> runTotalIO $ respond $ Wai.responseFile HttpTypes.status200 [("Content-Type", Text.encodeUtf8 contentType)] path Nothing
    _ -> runTotalIO $ respond $ Response.notFound

post :: ContentDecoder request -> ContentEncoder response -> (request -> Fx env Text response) -> Resource env
post = error "TODO"

get :: ContentEncoder response -> Fx env Text response -> Resource env
get = error "TODO"


-- ** Low-level
-------------------------

waiApplication :: Wai.Application -> Resource env
waiApplication application = Resource $ \ request respond -> runTotalIO $ application request respond


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

asJson :: (response -> Aeson.Value) -> ContentEncoder response
asJson = error "TODO"

asBinary :: (response -> CerealPut.Put) -> ContentEncoder response
asBinary = error "TODO"

asFile :: ContentEncoder FilePath
asFile = error "TODO"
