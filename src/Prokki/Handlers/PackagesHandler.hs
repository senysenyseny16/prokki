{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Resource (ResIO)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import Data.Conduit (ConduitT, ZipConduit (..), getZipConduit, runConduit, (.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, responseFile, responseStream)
import Prokki.Settings (Cache (..), Index (..))
import Prokki.Utils (getPath)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, openFile)

packagesHandler :: Request -> C.Manager -> Index -> Cache -> ResIO Response
packagesHandler req manager Index {..} Cache {..} = do
  let path = getPath req
      url = indexUrl <> path
      packagePath = cacheDir </> T.unpack path

  request <- C.parseRequest (T.unpack url)
  response <- C.http request {C.decompress = const False} manager
  let status = C.responseStatus response
      headers = C.responseHeaders response
      body = C.responseBody response

  isPackageCached <- liftIO $ doesFileExist packagePath

  if isPackageCached
    then do
      pure $ responseFile status headers packagePath Nothing
    else do
      withRunInIO \unlift ->
        pure $ responseStream status headers $ \rWrite rFlush -> do
          createDirectoryIfMissing True (takeDirectory packagePath)
          E.bracketOnError
            (openFile packagePath WriteMode)
            (cleanFile packagePath)
            ( \fHandle -> do
                E.catch
                  (unlift $ runConduit $ body .| writeAndRespond fHandle rWrite)
                  (\(_ :: IOError) -> cleanFile packagePath fHandle)
                hClose fHandle
                rFlush
            )

writeAndRespond :: (MonadIO m) => Handle -> (Builder -> IO ()) -> ConduitT ByteString o m ()
writeAndRespond fHandle rWrite =
  getZipConduit
    (ZipConduit (CC.sinkHandle fHandle) *> ZipConduit (CC.mapM_ (liftIO . rWrite . byteString)))

cleanFile :: FilePath -> Handle -> IO ()
cleanFile filePath handle = do
  hClose handle
  removeFile filePath `E.catch` \(_ :: IOError) -> pure ()
