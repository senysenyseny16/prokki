{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Cache (respondUsingCache) where

import Conduit (MonadResource, MonadThrow)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import Data.Conduit (ConduitT, ZipConduit (..), getZipConduit, runConduit, (.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseFile, responseStream)
import Prokki.Env (Cache (..))
import Prokki.Utils (tempExt)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, openFile)

respondUsingCache :: (MonadThrow m, MonadResource m, MonadUnliftIO m) => Cache -> C.Manager -> T.Text -> FilePath -> m Response
respondUsingCache Cache {..} manager url path = do
  let packagePath = cacheDir </> T.unpack (T.pack path)
      tempPackagePath = packagePath <> tempExt

  isPackageCached <- liftIO $ doesFileExist packagePath
  isTempFileExists <- liftIO $ doesFileExist tempPackagePath

  if isPackageCached
    then do
      pure $ responseFile status200 [] packagePath Nothing
    else do
      request <- C.parseRequest (T.unpack url)
      response <- C.http request manager
      let status = C.responseStatus response
          headers = C.responseHeaders response
          body = C.responseBody response

      if isTempFileExists -- (caching in progress) temporary solution
        then withRunInIO \unlift ->
          pure $ responseStream status headers $ \rWrite rFlush -> do
            unlift $ runConduit $ body .| CC.mapM_ (liftIO . rWrite . byteString)
            rFlush
        else withRunInIO \unlift ->
          pure $ responseStream status headers $ \rWrite rFlush -> do
            createDirectoryIfMissing True (takeDirectory packagePath)
            E.bracketOnError
              (openFile tempPackagePath WriteMode)
              (cleanFile tempPackagePath)
              ( \fHandle -> do
                  E.catch
                    (unlift $ runConduit $ body .| writeAndRespond fHandle rWrite)
                    (\(_ :: IOError) -> cleanFile tempPackagePath fHandle)
                  hClose fHandle
                  renameFile tempPackagePath packagePath
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
