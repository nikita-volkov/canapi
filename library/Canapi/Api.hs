module Canapi.Api where

import Canapi.Prelude
import Canapi.Types
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Canapi.Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Canapi.Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut


binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (env -> apiEnv) ->
  (err -> Text) ->
  (request -> Fx apiEnv err response) ->
  Api env
binary decoder encoder envProj errProj fx = do
  RequestParsing.methodIsPost
  RequestParsing.header "content-type" >>= guard . (==) "application/octet-stream"
  request <- hoist runTotalIO $ RequestParsing.bodyWithParser $ RequestBodyParsing.deserialize decoder
  response <- lift $ mapEnv envProj $ first errProj $ fx request
  return (
      ResponseBuilding.okayStatus <>
      ResponseBuilding.contentTypeHeader "application/octet-stream" <>
      ResponseBuilding.body (ResponseBodyBuilding.serialize (encoder response))
    )
