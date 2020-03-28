module Canapi.Application
where

import Canapi.Prelude
import Network.Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.HTTP.Media as HttpMedia
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Canapi.HttpAuthorizationParsing as HttpAuthorizationParsing
import qualified Canapi.RequestAccessor as RequestAccessor
import qualified Canapi.Response as Response
import qualified Canapi.RoutingTree as RoutingTree


concat :: [Application] -> Application
concat = foldl' alternate notFound

notFound :: Application
notFound _ = apply Response.notFound

alternate :: Application -> Application -> Application
alternate app1 app2 request respond =
  app1 request $ \ response1 -> 
  let statusCode1 = HttpTypes.statusCode (responseStatus response1)
    in if statusCode1 >= 400 && statusCode1 < 500
      then app2 request respond
      else respond response1

corsify :: Application -> Application
corsify = WaiCors.cors (const (Just policy)) where
  policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = WaiCors.simpleHeaders }

matchSegment :: Text -> Application -> Application
matchSegment segment subApp = 
  refineSegment (\ a -> if a == segment then Right subApp else Left Response.notFound)

attoparseSegment :: Attoparsec.Parser a -> (a -> Application) -> Application
attoparseSegment parser cont = 
  refineSegment (
      bimap (Response.plainBadRequest . fromString) cont .
      Attoparsec.parseOnly (parser <* Attoparsec.endOfInput)
    )

refineSegment :: (Text -> Either Response Application) -> Application
refineSegment = refineSegmentOr (const (apply Response.notFound))

refineSegmentOr :: Application -> (Text -> Either Response Application) -> Application
refineSegmentOr alternative refiner request = case pathInfo request of
  [""] -> alternative request
  segmentsHead : segmentsTail ->
    case refiner segmentsHead of
      Right cont -> cont (request { pathInfo = segmentsTail })
      Left err -> apply err
  _ -> alternative request

authorizing :: ByteString -> (Text -> Text -> Application) -> Application
authorizing realm cont request = case requestHeaders request & lookup "authorization" of
  Just bytes -> case HttpAuthorizationParsing.basicCredentials bytes of
    Right (username, password) -> cont username password request
    Left err -> apply (Response.plainBadRequest err)
  Nothing -> apply (Response.unauthorized realm)

whenNoSegmentsIsLeft :: Application -> Application
whenNoSegmentsIsLeft application request = if RequestAccessor.hasNoSegmentsLeft request
  then application request
  else apply Response.notFound


-- * Routing tree
-------------------------

routingTree :: RoutingTree.RoutingTree -> Application
routingTree (RoutingTree.RoutingTree segmentParser methodHandlerMap) = refineSegmentOr onNoSegment onSegment where
  onSegment = segmentParser >>> \ case
    Left Nothing -> Left Response.notFound
    Left (Just err) -> Left (Response.plainBadRequest err)
    Right nestedTree -> Right (routingTree nestedTree)
  onNoSegment = routingTreeMethodHandlerMap methodHandlerMap

routingTreeMethodHandlerMap :: RoutingTree.MethodHandlerMap -> Application
routingTreeMethodHandlerMap methodHandlerMap request =
  case Map.lookup (requestMethod request) methodHandlerMap of
    Just handler -> routingTreeHandler handler request
    Nothing -> apply Response.methodNotAllowed

routingTreeHandler :: RoutingTree.Handler -> Application
routingTreeHandler handler request respond = do
  requestBody <- strictRequestBody request
  response <- handler contentType accept (LazyByteString.toStrict requestBody)
  respond response
  where
    headers = requestHeaders request
    contentType = lookup "content-type" headers
    accept = lookup "accept" headers
