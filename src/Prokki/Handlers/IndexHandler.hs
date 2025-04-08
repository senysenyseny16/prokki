{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.IndexHandler (indexHandler) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Prokki.Utils (compress, replaceSubstring)

indexHandler :: Request -> IO Response
indexHandler req = do
  let path = T.intercalate "/" $ pathInfo req
      url = "https://pypi.org/" ++ T.unpack path

  manager <- C.newManager C.tlsManagerSettings
  request <- C.parseRequest url
  response <- C.httpLbs request manager
  let headers = HC.responseHeaders response
      body = replaceSubstring $ HC.responseBody response
      cbody = compress body
      bodyLength = LBS.length cbody
      newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

  return $ responseLBS (HC.responseStatus response) newHeaders cbody
