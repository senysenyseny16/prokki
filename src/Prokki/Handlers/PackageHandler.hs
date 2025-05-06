module Prokki.Handlers.PackageHandler (packageHandler) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Text as T
import Network.HTTP.Conduit (Manager)
import Network.Wai (Request, Response)
import Prokki.Cache (respondUsingCache)
import Prokki.Env (WithCache, WithManager, grab)
import Prokki.Type (Cache, Index (..), Path)
import Prokki.Utils (escapeUnreservedChars)
import System.FilePath ((</>))

packageHandler ::
  (MonadResource m, MonadThrow m, MonadUnliftIO m, WithManager env m, WithCache env m) =>
  Request ->
  Index ->
  Path ->
  m Response
packageHandler _ Index {..} reqPath = do
  cache <- grab @Cache
  manager <- grab @Manager
  let reqPath' = T.intercalate "/" reqPath
      url = origin <> "/" <> escapeUnreservedChars reqPath'
      filePath = T.unpack index </> T.unpack reqPath'

  respondUsingCache cache manager url filePath
