module Prokki.Middleware.RequestLogger (logRequests) where

import Colog (LogAction, Message, Msg (..), Severity (..), (<&))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack, callStack)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)

logRequests :: (HasCallStack) => LogAction IO Message -> Middleware
logRequests logAction app req respond = do
  let method = decodeUtf8 (requestMethod req)
      path = decodeUtf8 (rawPathInfo req)

  app req \res -> do
    let status = statusCode (responseStatus res)
        text =
          T.concat
            [ "[HTTP] ",
              method,
              " ",
              path,
              " -> ",
              T.pack (show status)
            ]
    logAction <& (Msg {msgText = text, msgSeverity = Info, msgStack = callStack})
    respond res
