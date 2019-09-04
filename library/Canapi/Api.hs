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
