module Prokki.Storage.S3.Http (unchunk) where

import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Conduit as HC
import Network.HTTP.Types.Header (hContentLength)
import Text.Read (readMaybe)

-- Amazonka sends AWS-chunked PutObject bodies as RequestBodyStreamChunked
-- with Content-Length already set explicitly; http-client then also adds
-- Transfer-Encoding: chunked, so strict S3-compatible servers (e.g. Ceph RGW)
-- see two conflicting length headers and reject the request outright.
unchunk :: HC.Request -> HC.Request
unchunk req
  | HC.RequestBodyStreamChunked popper <- HC.requestBody req,
    Just n <- lookup hContentLength (HC.requestHeaders req) >>= readMaybe . BS8.unpack =
      req
        { HC.requestBody = HC.RequestBodyStream n popper,
          HC.requestHeaders = filter ((/= hContentLength) . fst) (HC.requestHeaders req)
        }
  | otherwise = req
