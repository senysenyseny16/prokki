{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Utils (compress, getPath)

packagesHandler :: C.Manager -> Request -> IO Response
packagesHandler manager req = do
  let path = getPath req
      url = "https://pypi.org/" ++ path

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
