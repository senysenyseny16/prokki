module Prokki.Middleware.RequestLogger (logRequests) where

import Colog (LogAction, Message, Msg (..), Severity (..), (<&))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack, callStack)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, Request, Response, httpVersion, rawPathInfo, remoteHost, requestMethod, responseStatus)

logRequests :: (HasCallStack) => LogAction IO Message -> Middleware
logRequests logAction app req respond = app req respond'
  where
    respond' res = do
      let status = statusCode (responseStatus res)
          severity = if status == 200 || status == 301 then Info else Warning
      logAction <& (Msg {msgText = requestMsg req res, msgSeverity = severity, msgStack = callStack})
      respond res

requestMsg :: Request -> Response -> T.Text
requestMsg req res =
  let method = decodeUtf8 (requestMethod req)
      path = decodeUtf8 (rawPathInfo req)
      host = stripPort $ T.pack $ show (remoteHost req)
      status = T.pack $ show (statusCode (responseStatus res))
      httpVer = T.pack $ show (httpVersion req)
   in T.intercalate " " [host, method, path, httpVer, status]

stripPort :: T.Text -> T.Text
stripPort = T.takeWhile (/= ':')
{-# INLINE stripPort #-}
