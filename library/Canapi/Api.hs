module Canapi.Api where

import Canapi.Prelude
import Canapi.Types
import qualified Strelka.RequestParsing as RequestParsing
import qualified Strelka.ResponseBuilding as ResponseBuilding
import qualified Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Canapi.Parser as Parser
import qualified Canapi.Strelka.RequestBodyParsing as RequestBodyParsing
import qualified Canapi.Strelka.ResponseBodyBuilding as ResponseBodyBuilding
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Serialize.Put as CerealPut
import qualified Most.Cereal.Get as CerealGet
import qualified Most.Cereal.Put as CerealPut
import qualified Most.Types as MostTypes


binary ::
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (env -> actionEnv) ->
  (err -> Text) ->
  (request -> Action actionEnv err response) ->
  Api env
binary decoder encoder envProj errProj action = do
  RequestParsing.methodIsPost
  RequestParsing.header "content-type" >>= guard . (==) "application/octet-stream"
  request <- RequestParsing.bodyWithParser (RequestBodyParsing.deserialize decoder)
  response <- Parser.action envProj errProj (action request)
  return
    (
      ResponseBuilding.okayStatus <>
      ResponseBuilding.contentTypeHeader "application/octet-stream" <>
      ResponseBuilding.body (ResponseBodyBuilding.serialize (encoder response))
    )

{-|
Versioned binary endpoint.
-}
most ::
  Word32 ->
  CerealGet.Get request ->
  (response -> CerealPut.Put) ->
  (env -> actionEnv) ->
  (err -> Text) ->
  (request -> Action actionEnv err response) ->
  Api env
most version decoder encoder envProj errProj action = 
  binary
    (CerealGet.versionedRequestDecoding version decoder)
    (CerealPut.versionedResponse version encoder)
    (envProj)
    (errProj)
    (\ case
      MostTypes.SupportedVersionedRequestDecoding request ->
        fmap MostTypes.SupportedVersionedResponse (action request)
      MostTypes.UnsupportedVersionedRequestDecoding _ ->
        pure MostTypes.UnsupportedVersionedResponse)
