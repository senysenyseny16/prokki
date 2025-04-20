{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prokki.Handlers.SimpleHandler (simpleHandler) where

import Control.Monad.Reader (ask)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, isSecure, requestHeaderHost, requestHeaders, responseLBS)
import Prokki.Env (Env (..), Index (..))
import Prokki.Monad (ProkkiM)
import Prokki.Utils (compress, getPath, replacePackageLink)

simpleHandler :: Request -> ProkkiM Response
simpleHandler req = do
  Env {..} <- ask
  let Index {..} = index
      url = indexUrl <> getPath req
      reqHeaders = requestHeaders req
      xfp = lookup "X-Forwarded-Proto" reqHeaders
      scheme = case xfp of
        Just proto -> decodeUtf8 proto
        Nothing -> if isSecure req then "https" else "http"
      host = fromMaybe (error "Host header now found") $ requestHeaderHost req
      addr = scheme <> "://" <> decodeUtf8 host

  request <- C.parseRequest (T.unpack url)
  response <- C.httpLbs request manager
  let headers = C.responseHeaders response
      newBody = compress (replacePackageLink (C.responseBody response) addr)
      bodyLength = LBS.length newBody
      newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

  pure $ responseLBS (C.responseStatus response) newHeaders newBody
