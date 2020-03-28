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

type MethodHandlerMap = Map ByteString Handler

type Handler = Maybe ByteString -> Maybe ByteString -> ByteString -> IO Wai.Response


-- * Instances
-------------------------

instance Semigroup RoutingTree where
  (<>) (RoutingTree segmentParser1 methodHandlerMap1) (RoutingTree segmentParser2 methodHandlerMap2) =
    RoutingTree segmentParser methodHandlerMap
    where
      segmentParser input = case segmentParser1 input of
        Left _ -> segmentParser2 input
        Right routingTree -> Right routingTree
      methodHandlerMap = Map.union methodHandlerMap1 methodHandlerMap2

instance Monoid RoutingTree where
  mempty = RoutingTree (const (Left Nothing)) Map.empty
  mappend = (<>)
