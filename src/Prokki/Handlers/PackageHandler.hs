module Prokki.Handlers.PackageHandler (packageHandler) where

import Colog (Message, WithLog)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Text as T
import Network.HTTP.Conduit (Manager)
import Network.Wai (Request, Response)
import Prokki.Cache (respondUsingCache)
import Prokki.Env (WithCache, WithManager, WithResponseTimeout, grab)
import Prokki.Type (Cache, Index (..), Path)
import Prokki.Utils (escapeUnreservedChars)
import System.FilePath ((</>))

-- | The handler caches packages, including their metadata.
-- If a package is already cached, the handler returns it directly;
-- otherwise, it simultaneously initiates downloading the package from the index,
-- caching it, and delivering it to the client.
packageHandler ::
  ( MonadResource m,
    MonadCatch m,
    MonadUnliftIO m,
    WithManager env m,
    WithCache env m,
    WithLog env Message m,
    WithResponseTimeout env m
  ) =>
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
