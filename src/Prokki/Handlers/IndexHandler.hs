{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.IndexHandler (indexHandler) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Utils (compress, getPath, replaceSubstring)

indexHandler :: C.Manager -> Request -> IO Response
indexHandler manager req = do
  let path = getPath req
      url = "https://pypi.org/" ++ path

  request <- C.parseRequest url
  response <- C.httpLbs request manager
  let headers = C.responseHeaders response
      body = replaceSubstring $ C.responseBody response
      cbody = compress body
      bodyLength = LBS.length cbody
      newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

  pure $ responseLBS (C.responseStatus response) newHeaders cbody
