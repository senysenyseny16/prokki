{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.ErrorHandler (errorHandler) where

import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)

errorHandler :: Request -> C.Manager -> IO Response
errorHandler _ _ = pure $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
