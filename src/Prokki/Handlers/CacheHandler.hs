{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.CacheHandler (
  cacheHandler
) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (
    status200,
    status204,
    status400,
    status404,
    status405
  )
import Network.HTTP.Types.Method (methodDelete, methodGet)
import Network.Wai (Request, Response, requestMethod, responseLBS)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Settings (Cache(..))
import System.Directory (
    removeFile,
    removeDirectoryRecursive,
    createDirectory,
    listDirectory,
    doesDirectoryExist,
    doesFileExist
  )
import System.FilePath ((</>), makeRelative)
import Data.List (isSuffixOf)
import Data.Text (Text)

cacheHandler :: [Text] -> Request -> C.Manager -> Cache -> IO Response
cacheHandler subPath req manager cache@Cache{..} =
  case subPath of
    ("package" : packageParts) -> handleDeletePackage packageParts req cacheDir
    ["all"]                     -> handleDeleteAll req cacheDir
    ["size"]                    -> handleGetSize req cacheDir
    _                           -> errorHandler req manager

-- | DELETE /cache/package/:packageName
handleDeletePackage :: [Text] -> Request -> FilePath -> IO Response
handleDeletePackage packageParts req cacheDir = do
  if requestMethod req /= methodDelete
    then methodNotAllowed
    else do
      let packagePath = T.unpack $ T.intercalate "/" packageParts
          fullPath = cacheDir </> packagePath
      
      isValid <- validatePath cacheDir fullPath
      if not isValid
        then badRequest "Invalid path"
        else do
          packageExists <- doesFileExist fullPath
          if not packageExists
            then notFound "Package not found"
            else deletePackageAndMetadata fullPath

-- | DELETE /cache/all
handleDeleteAll :: Request -> FilePath -> IO Response
handleDeleteAll req cacheDir = do
  if requestMethod req /= methodDelete
    then methodNotAllowed
    else do
      removeCacheDirectory cacheDir
      return $ responseLBS status200 [] "Cache cleared"

-- | GET /cache/size
handleGetSize :: Request -> FilePath -> IO Response
handleGetSize req cacheDir = do
  if requestMethod req /= methodGet
    then methodNotAllowed
    else do
      size <- getCacheSize cacheDir
      putStrLn $ "Count of files in cache directory: " ++ show size  -- Logging
      return $ responseLBS status200 [] (BSC8.pack $ show size)

methodNotAllowed :: IO Response
methodNotAllowed = return $ responseLBS status405 [] "Method Not Allowed"

badRequest :: BSC8.ByteString -> IO Response
badRequest msg = return $ responseLBS status400 [] msg

notFound :: BSC8.ByteString -> IO Response
notFound msg = return $ responseLBS status404 [] msg

deletePackageAndMetadata :: FilePath -> IO Response
deletePackageAndMetadata fullPath = do
  removeFileWithLogging fullPath "Failed to delete package file"
  
  let metadataPath = fullPath ++ ".metadata"
  
  metadataExists <- doesFileExist metadataPath
  when metadataExists $
    removeFileWithLogging metadataPath "Failed to delete metadata file"
  
  return $ responseLBS status204 [] BSC8.empty

removeCacheDirectory :: FilePath -> IO ()
removeCacheDirectory cacheDir = do
  removeDirectoryRecursive cacheDir
    `catch` handleIOException "Failed to delete cache directory"
  createDirectory cacheDir
    `catch` handleIOException "Failed to recreate cache directory"


getCacheSize :: FilePath -> IO Int
getCacheSize dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then return 0
    else do
      contents <- listDirectory dir
      counts <- mapM (\item -> do
                        let path = dir </> item
                        isFile <- doesFileExist path
                        if isFile
                          then if not (T.isSuffixOf ".metadata" (T.pack item))
                                 then return 1
                                 else return 0
                          else getCacheSize path
                      ) contents
      return (sum counts)


hasMetadataExtension :: FilePath -> Bool
hasMetadataExtension filename = ".metadata" `isSuffixOf` filename

validatePath :: FilePath -> FilePath -> IO Bool
validatePath base path = 
  return $ not (".." `isSuffixOf` path) && makeRelative base path /= path

removeFileWithLogging :: FilePath -> String -> IO ()
removeFileWithLogging path errorMsg =
  removeFile path `catch` (\(e :: IOException) -> putStrLn $ errorMsg ++ ": " ++ show e)

handleIOException :: String -> IOException -> IO ()
handleIOException msg e = putStrLn $ msg ++ ": " ++ show e
