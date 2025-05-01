{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Text as T
import Network.HTTP.Conduit (Manager)
import Network.Wai (Request, Response)
import Prokki.Cache (respondUsingCache)
import Prokki.Env (WithCache, WithIndex, WithManager, grab)
import Prokki.Type (Cache, Index (..))
import Prokki.Utils (getPath)

packagesHandler ::
  (MonadResource m, MonadThrow m, MonadUnliftIO m, WithIndex env m, WithCache env m, WithManager env m) =>
  Request ->
  m Response
packagesHandler req = do
  Index {..} <- grab @Index
  cache <- grab @Cache
  manager <- grab @Manager
  let path = getPath req
      url = indexUrl <> path

  respondUsingCache cache manager url (T.unpack path)
