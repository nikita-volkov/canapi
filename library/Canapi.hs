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
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Canapi.Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Canapi.Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Canapi.Strelka.IO as StrelkaIO
import qualified Canapi.Optima.ParamGroup as Optima
import qualified Optima
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut


-- * IO
-------------------------

serve :: Word16 -> Provider Text env -> Resource env -> IO ()
serve port envProvider (Resource parser) = StrelkaIO.serve port envProvider parser

{-|
Parse CLI args and produce an exception-free IO-action, which runs a server.
-}
serveParsingCliArgs :: Text -> Optima.ParamGroup envArgs -> (envArgs -> Provider Text env) -> Resource env -> IO ()
serveParsingCliArgs appDesc envParamGroup provider api = do
  (port, envArgs) <- Optima.params appDesc $ Optima.group "" $ Optima.settings envParamGroup
  serve port (provider envArgs) api


-- * Resource
-------------------------

newtype Resource env = Resource (RequestParsing.Parser (Fx env Text) ResponseBuilding.Builder)

instance Semigroup (Resource env) where
  (<>) (Resource a) (Resource b) = Resource (a <|> b)

instance Monoid (Resource env) where
  mempty = Resource empty
  mappend = (<>)

instance Contravariant Resource where
  contramap f (Resource a) = Resource (a & hoist (mapEnv f))

atSegment :: Text -> Resource env -> Resource env
atSegment segment (Resource nested) = Resource $ do
  RequestParsing.segmentIs segment
  nested

binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (env -> apiEnv) ->
  (err -> Text) ->
  (request -> Fx apiEnv err response) ->
  Resource env
binary decoder encoder envProj errProj fx = Resource $ do
  RequestParsing.methodIsPost
  RequestParsing.header "content-type" >>= guard . (==) "application/octet-stream"
  request <- hoist runTotalIO $ RequestParsing.bodyWithParser $ RequestBodyParsing.deserialize decoder
  response <- lift $ mapEnv envProj $ first errProj $ fx request
  return (
      ResponseBuilding.okayStatus <>
      ResponseBuilding.contentTypeHeader "application/octet-stream" <>
      ResponseBuilding.body (ResponseBodyBuilding.serialize (encoder response))
    )