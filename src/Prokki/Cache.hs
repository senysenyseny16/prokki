{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Cache (respondWithCache) where

import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import Data.Conduit (ConduitT, ZipConduit (..), getZipConduit, runConduit, (.|))
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (Response, responseFile, responseStream)
import Prokki.Env (Cache (..))
import Prokki.Utils (tempExt)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, openFile)

respondWithCache :: (MonadUnliftIO m) => Cache -> FilePath -> Status -> ResponseHeaders -> ConduitT () ByteString m () -> m Response
respondWithCache Cache {..} path status headers body = do
  let packagePath = cacheDir </> T.unpack (T.pack path)
      tempPackagePath = packagePath <> tempExt

  isPackageCached <- liftIO $ doesFileExist packagePath
  isTempFileExists <- liftIO $ doesFileExist tempPackagePath

  if isPackageCached
    then do
      pure $ responseFile status headers packagePath Nothing
    else
      if isTempFileExists -- temporary solution
        then do
          withRunInIO \unlift ->
            pure $ responseStream status headers $ \rWrite rFlush -> do
              unlift $ runConduit $ body .| CC.mapM_ (liftIO . rWrite . byteString)
              rFlush
        else do
          withRunInIO \unlift ->
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
