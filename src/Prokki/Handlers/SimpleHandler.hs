{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Prokki.Handlers.SimpleHandler (simpleHandler) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength)
import Network.Wai (Request, Response, isSecure, requestHeaderHost, requestHeaders, responseLBS)
import Prokki.Env (WithIndex, WithManager, grab)
import Prokki.Type (Index (..))
import Prokki.Utils (getPath, replacePackageLink)

simpleHandler :: (MonadIO m, WithManager env m, WithIndex env m) => Request -> m Response
simpleHandler req = do
  Index {..} <- grab @Index
  manager <- grab @Manager
  let url = indexUrl <> getPath req
      reqHeaders = requestHeaders req
      xfp = lookup "X-Forwarded-Proto" reqHeaders
      scheme = case xfp of
        Just proto -> decodeUtf8 proto
        Nothing -> if isSecure req then "https" else "http"
      host = fromMaybe (error "Host header now found") $ requestHeaderHost req
      addr = scheme <> "://" <> decodeUtf8 host

  request <- liftIO $ C.parseRequest (T.unpack url)
  response <- C.httpLbs request manager
  let headers = C.responseHeaders response
      newBody = replacePackageLink (C.responseBody response) addr
      bodyLength = LBS.length newBody
      newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

  pure $ responseLBS (C.responseStatus response) newHeaders newBody
