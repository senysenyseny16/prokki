{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.ErrorHandler (errorHandler) where

import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Monad (ProkkiM)

errorHandler :: Request -> ProkkiM Response
errorHandler _ = pure $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
