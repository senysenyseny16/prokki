{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response)
import Prokki.Cache (respondWithCache)
import Prokki.Env (Env (..), Index (..))
import Prokki.Monad (ProkkiM)
import Prokki.Utils (getPath)

packagesHandler :: Request -> ProkkiM Response
packagesHandler req = do
  Env {..} <- ask
  let Index {..} = index
      path = getPath req
      url = indexUrl <> path

  request <- C.parseRequest (T.unpack url)
  response <- C.http request manager
  let status = C.responseStatus response
      headers = C.responseHeaders response
      body = C.responseBody response

  respondWithCache cache (T.unpack path) status headers body
