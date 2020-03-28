module Canapi.RequestAccessor
where

import Canapi.Prelude
import Canapi.Data
import qualified Canapi.NetworkIp as NetworkIp
import qualified Canapi.HeaderParsing as HeaderParsing
import qualified Network.Wai as Wai
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Control.Foldl as Foldl


requestMetadata :: Wai.Request -> RequestMetadata
requestMetadata request =
  RequestMetadata ip userAgent referer contentType contentTypeHeader acceptHeader
  where
    sockAddr = Wai.remoteHost request
    ip = case NetworkIp.sockAddrIP sockAddr of
      Just a -> a
      Nothing -> error (
          "Warp has set an unexpected remoteHost address: " <> show sockAddr <> ". " <>
          "Please report this to the maintainers of the \"canapi\" package."
        )
    userAgent = fmap Text.decodeLatin1 (Wai.requestHeaderUserAgent request)
    referer = fmap Text.decodeLatin1 (Wai.requestHeaderReferer request)
    HeadersOfInterest contentTypeHeader acceptHeader = headersOfInterest request
    contentType = contentTypeHeader >>= HeaderParsing.contentType

headerMap :: Wai.Request -> HashMap ByteString ByteString
headerMap = Wai.requestHeaders >>> fmap (first CaseInsensitive.foldedCase) >>> HashMap.fromList

headersOfInterest :: Wai.Request -> HeadersOfInterest
headersOfInterest = Wai.requestHeaders >>> Foldl.fold fold where
  fold = HeadersOfInterest <$> Foldl.lookup "content-type" <*> Foldl.lookup "accept"

hasNoSegmentsLeft :: Wai.Request -> Bool
hasNoSegmentsLeft = Wai.pathInfo >>> \ case
  [""] -> True
  [] -> True
  _ -> False
