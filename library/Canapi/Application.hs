module Canapi.Application
where

import Canapi.Prelude
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.HTTP.Types as HttpTypes
import qualified Canapi.Response as Response


concat :: [Wai.Application] -> Wai.Application
concat = foldl1' alternate

alternate :: Wai.Application -> Wai.Application -> Wai.Application
alternate app1 app2 request respond =
  app1 request $ \ response1 -> 
  let statusCode1 = HttpTypes.statusCode (Wai.responseStatus response1)
    in if statusCode1 >= 400 && statusCode1 < 500
      then app2 request respond
      else respond response1

corsify :: Wai.Application -> Wai.Application
corsify = WaiCors.cors (const (Just policy)) where
  policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = WaiCors.simpleHeaders }
