{-|
An intermediate model, which provides for construction of an efficient application.
-}
module Canapi.RoutingTree
where

import Canapi.Prelude hiding (Handler)
import qualified Data.Map.Strict as Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Canapi.HttpAuthorizationParsing as HttpAuthorizationParsing
import qualified Canapi.RequestAccessor as RequestAccessor


data RoutingTree = RoutingTree SegmentParser MethodHandlerMap

type SegmentParser = Text -> Either (Maybe Text) RoutingTree

type MethodHandlerMap = Map ByteString ContentTypeHandlerMap

type ContentTypeHandlerMap = MapWithDefault HttpMedia.MediaType AcceptHandlerMap

type AcceptHandlerMap = MapWithDefault HttpMedia.MediaType Handler

data MapWithDefault k v = MapWithDefault v (Map k v)

type Handler = ByteString -> IO (Either Err Wai.Response)

data Err = ClientErr Text | ServerErr Text


-- * Instances
-------------------------

instance Semigroup RoutingTree where
  (<>) (RoutingTree segmentParser1 methodHandlerMap1) (RoutingTree segmentParser2 methodHandlerMap2) =
    RoutingTree segmentParser methodHandlerMap
    where
      segmentParser input = case segmentParser1 input of
        Left _ -> segmentParser2 input
        Right routingTree -> Right routingTree
      methodHandlerMap = Map.unionWith methodHandlerMapUnion methodHandlerMap1 methodHandlerMap2
      
methodHandlerMapUnion
  (MapWithDefault defaultAcceptHandlerMap1 contentTypeHandlerMap1)
  (MapWithDefault defaultAcceptHandlerMap2 contentTypeHandlerMap2) =
  MapWithDefault defaultAcceptHandlerMap1
    (Map.unionWith contentTypeHandlerMapUnion contentTypeHandlerMap1 contentTypeHandlerMap2)

contentTypeHandlerMapUnion
  (MapWithDefault defaultContentTypeHandlerMap1 handlerMap1)
  (MapWithDefault defaultContentTypeHandlerMap2 handlerMap2) =
  MapWithDefault defaultContentTypeHandlerMap1
    (Map.unionWith handlerMapUnion handlerMap1 handlerMap2)

handlerMapUnion handler1 handler2 input =
  handler1 input >>= \ case
    Left _ -> handler2 input
    Right response -> return (Right response)
