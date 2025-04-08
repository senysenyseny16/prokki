{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Prokki.Utils (compress)

packagesHandler :: Request -> IO Response
packagesHandler req = do
  let path = T.intercalate "/" $ pathInfo req
      url = "https://pypi.org/" ++ T.unpack path

  manager <- C.newManager C.tlsManagerSettings
  request <- C.parseRequest url
  response <- C.httpLbs request manager

  let headers = HC.responseHeaders response
      encoding = lookup "Content-Encoding" headers
      body = HC.responseBody response
      newBody = case encoding of
        Just "gzip" -> compress body
        _ -> body
      bodyLength = LBS.length newBody
      newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

  return $ responseLBS (HC.responseStatus response) newHeaders newBody
