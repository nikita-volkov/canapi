{-|
An intermediate model, which provides for construction of an efficient application.
-}
module Canapi.RoutingTree
where

import Canapi.Prelude hiding (Handler)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Canapi.HttpAuthorizationParsing as HttpAuthorizationParsing
import qualified Canapi.RequestAccessor as RequestAccessor


data RoutingTree = RoutingTree (Text -> Either Text RoutingTree) MethodHandlerMap

type MethodHandlerMap = Map ByteString ContentTypeHandlerMap

type ContentTypeHandlerMap = MapWithDefault HttpMedia.MediaType AcceptHandlerMap

type AcceptHandlerMap = MapWithDefault HttpMedia.MediaType Handler

data MapWithDefault k v = MapWithDefault v (Map k v)

type Handler = ByteString -> IO (Either Err Wai.Response)

data Err = ClientErr Text | ServerErr Text
