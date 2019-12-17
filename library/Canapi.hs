module Canapi (
    -- * IO
    serve,
    serveParsingCliArgs,
    -- * Resource
    Resource,
    atSegment,
    binary,
  ) where

import Canapi.Prelude
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Optima
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text as Text


-- * IO
-------------------------

serve :: Resource env -> Word16 -> Bool -> Provider Text env -> IO Void
serve (Resource parser) port cors envProvider =
  runFx $ handleErr (fail . Text.unpack) $ provideAndUse envProvider $ handleEnv $ \ env ->
  runTotalIO $ do
    Warp.run (fromIntegral port) $ (if cors then WaiCors.simpleCors else id) $ \ request cont -> do
      waiResponse <- runFx $ handleErr (fail . Text.unpack) $ provideAndUse (pure env) $ parser request
      cont waiResponse
    fail "The server stopped"

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Resource env -> Text -> Optima.ParamGroup envArgs -> (envArgs -> Provider Text env) -> IO Void
serveParsingCliArgs api appDesc envParamGroup provider = do
  (port, cors, envArgs) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve api port cors (provider envArgs)


-- * Resource
-------------------------

newtype Resource env = Resource (Wai.Request -> Fx env Text Wai.Response)

instance Semigroup (Resource env) where
  (<>) (Resource a) (Resource b) = Resource $ \ request -> do
    response1 <- a request
    let statusCode1 = HttpTypes.statusCode (Wai.responseStatus response1)
    if statusCode1 >= 400 && statusCode1 < 500
      then b request
      else return response1

instance Monoid (Resource env) where
  mempty = Resource (const (return (Wai.responseLBS HttpTypes.status404 [] "")))
  mappend = (<>)
  mconcat = \ case
    a : [] -> a
    a : b -> a <> mconcat b
    [] -> mempty

instance Contravariant Resource where
  contramap f (Resource a) = Resource (mapEnv f . a)

atSegment :: Text -> Resource env -> Resource env
atSegment expectedSegment (Resource nested) = Resource $ \ request ->
  case Wai.pathInfo request of
    segmentsHead : segmentsTail -> if expectedSegment == segmentsHead
      then nested (request { Wai.pathInfo = segmentsTail })
      else return (Wai.responseLBS HttpTypes.status404 [] "")
    _ -> return (Wai.responseLBS HttpTypes.status404 [] "")

{-|
Binary protocol resource.

Only supports to the @POST@ method.
-}
binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (env -> apiEnv) ->
  (err -> Text) ->
  (request -> Fx apiEnv err response) ->
  Resource env
binary decoder encoder envProj errProj fx = Resource $ \ request ->
  if Wai.requestMethod request == HttpTypes.methodPost
    then do
      requestBody <- runTotalIO $ Wai.strictRequestBody request
      case CerealGet.runGetLazy decoder requestBody of
        Right request -> do
          response <- mapEnv envProj $ first errProj $ fx request
          let
            waiResponse =
              Wai.responseBuilder
                HttpTypes.status200
                []
                (CerealPut.execPut (encoder response))
            in return waiResponse
        Left err -> return (Wai.responseLBS HttpTypes.status400 [] (fromString err))
    else return (Wai.responseLBS HttpTypes.status405 [] "")
