module Canapi.RequestMetadata
where

import Canapi.Prelude
import qualified Canapi.NetworkIp as NetworkIp
import qualified Network.Wai as Wai
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


data RequestMetadata = RequestMetadata {
    ip :: IP,
    userAgent :: Maybe Text,
    referer :: Maybe Text
  }

fromWaiRequest :: Wai.Request -> RequestMetadata
fromWaiRequest request =
  RequestMetadata ip userAgent referer
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
