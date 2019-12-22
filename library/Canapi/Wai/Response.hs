module Canapi.Wai.Response
where

import Canapi.Prelude
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Middleware.Cors as WaiCors
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


notFound = Wai.responseLBS HttpTypes.status404 [] ""
