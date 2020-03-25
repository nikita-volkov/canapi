module Canapi.Application
where

import Canapi.Prelude
import Network.Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.HTTP.Types as HttpTypes
import qualified Canapi.Response as Response
import qualified Data.Attoparsec.Text as Attoparsec


concat :: [Application] -> Application
concat = foldl1' alternate

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
refineSegment refiner request = case pathInfo request of
  segmentsHead : segmentsTail ->
    case refiner segmentsHead of
      Right cont -> cont (request { pathInfo = segmentsTail })
      Left err -> apply err
  _ -> apply Response.notFound
