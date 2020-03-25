module Canapi where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi.Data
import qualified Canapi.Application as Application
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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Attoparsec.Data as AttoparsecData
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Foldl as Foldl
import qualified Fx


data Resource env params =
  AtResource Text [Resource env params] |
  forall segment. ByResource [SegmentParser segment] [Resource env (segment, params)] |
  forall identity. AuthenticatedResource (Text -> Text -> Fx env Err (Maybe identity)) [Resource env (identity, params)] |
  forall request response. HandlerResource HttpTypes.Method [Receiver request] [Responder response] (params -> request -> Fx env Err response) |
  RedirectResource Int Text |
  FileSystemResource FilePath

data Receiver a = Receiver HttpMedia.MediaType (ByteString -> Either Text a)

data Responder a = Responder HttpMedia.MediaType (a -> Wai.Response)

data SegmentParser a = SegmentParser Text (Attoparsec.Parser a)

data Err =
  ClientErr Text |
  ServerErr Text


-- * Execution
-------------------------

serve :: [Resource env ()] -> Word16 -> Bool -> Fx env err Void
serve resourceList port cors =
  Fx.runTotalIO $ \ env -> do
    Warp.run (fromIntegral port) (corsify (buildWaiApplication env () resourceList))
    fail "Warp unexpectedly stopped"
  where
    corsify = if cors then Application.corsify else id

buildWaiApplication :: env -> params -> [Resource env params] -> Wai.Application
buildWaiApplication env params = Application.concat . fmap fromResource where
  fromResource = \ case
    AtResource segment subResourceList ->
      Application.matchSegment segment (buildWaiApplication env params subResourceList)
    ByResource segmentParserList subResourceList -> let
      parser = segmentParserList & fmap (\ (SegmentParser _ p) -> p) & asum
      in
        Application.attoparseSegment parser $ \ segment ->
        buildWaiApplication env (segment, params) subResourceList
    HandlerResource method receiverList responderList handler -> \ request -> let
      headers = Wai.requestHeaders request
      (acceptHeader, contentTypeHeader) = headers & Foldl.fold ((,) <$> Foldl.lookup "accept" <*> Foldl.lookup "content-type")
      in if Wai.requestMethod request /= method
        then apply Response.methodNotAllowed
        else case runReceiverList receiverList contentTypeHeader of
          Nothing -> apply Response.unsupportedMediaType
          Just decoder -> case runResponderList responderList acceptHeader contentTypeHeader of
            Nothing -> apply Response.notAcceptable
            Just encoder -> \ respond -> do
              requestBody <- Wai.strictRequestBody request
              case decoder (LazyByteString.toStrict requestBody) of
                Left err -> respond (Response.plainBadRequest err)
                Right request ->
                  Fx.runFxHandling
                    (\ case
                      ServerErr err -> do
                        Text.hPutStrLn stderr err
                        respond Response.internalServerError
                      ClientErr err -> respond (Response.plainBadRequest err))
                    (Fx.provideAndUse (pure env) (do
                      response <- handler params request
                      Fx.runTotalIO (const (respond (encoder response)))))
    _ -> error "TODO"

runReceiverList :: [Receiver request] -> Maybe ByteString -> Maybe (ByteString -> Either Text request)
runReceiverList receiverList = \ case
  Just contentType -> HttpMedia.mapContentMedia mediaAssocList contentType
  Nothing -> case receiverList of
    Receiver _ refiner : _ -> Just refiner
    _ -> Nothing
  where
    mediaAssocList = receiverList & fmap (\ (Receiver a b) -> (a, b))

runResponderList :: [Responder response] -> Maybe ByteString -> Maybe ByteString -> Maybe (response -> Wai.Response)
runResponderList responderList acceptHeader contentTypeHeader =
  byAccept <|> byContentType <|> byHead
  where
    byAccept = acceptHeader >>= HttpMedia.mapAcceptMedia mediaAssocList
    byContentType = contentTypeHeader >>= HttpMedia.mapContentMedia mediaAssocList
    byHead = case mediaAssocList of
      (_, a) : _ -> Just a
      _ -> Nothing
    mediaAssocList = responderList & fmap (\ (Responder a b) -> (a, b))


-- * DSL
-------------------------

at :: Text -> [Resource env params] -> Resource env params
at = AtResource

by :: [SegmentParser segment] -> [Resource env (segment, params)] -> Resource env params
by = ByResource

head :: (params -> Fx env Err ()) -> Resource env params
head handler = HandlerResource "head" [] [] (\ params () -> handler params)

get :: [Responder response] -> (params -> Fx env Err response) -> Resource env params
get responder handler = HandlerResource "get" [] responder (\ params () -> handler params)

post :: [Receiver request] -> [Responder response] -> (params -> request -> Fx env Err response) -> Resource env params
post = HandlerResource "post"

put :: [Receiver request] -> [Responder response] -> (params -> request -> Fx env Err response) -> Resource env params
put = HandlerResource "put"

delete :: [Responder response] -> (params -> Fx env Err response) -> Resource env params
delete responder handler = HandlerResource "delete" [] responder (\ params () -> handler params)


-- * Instances
-------------------------

instance Functor SegmentParser where
  fmap mapper (SegmentParser description parser) = SegmentParser description (fmap mapper parser)

instance Functor Receiver where
  fmap = error "TODO"

instance Applicative Receiver where
  pure = error "TODO"
  (<*>) = error "TODO"

instance Contravariant Responder where
  contramap mapper (Responder mediaType response) = Responder mediaType (response . mapper)
