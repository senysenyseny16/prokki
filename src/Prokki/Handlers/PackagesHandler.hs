{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import Network.Wai (Request, Response)
import Prokki.Cache (respondUsingCache)
import Prokki.Env (Env (..), Index (..))
import Prokki.Monad (ProkkiM)
import Prokki.Utils (getPath)

packagesHandler :: Request -> ProkkiM Response
packagesHandler req = do
  Env {..} <- ask
  let Index {..} = index
      path = getPath req
      url = indexUrl <> path

  respondUsingCache cache manager url (T.unpack path)
