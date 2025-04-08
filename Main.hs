{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Simple
import Network.HTTP.Types (status200)
import Network.URI (parseURI, uriAuthority, uriPath, uriScheme)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Parsec
import Text.Parsec.String (Parser)

cacheDir :: FilePath
cacheDir = "cache"

-- Get the cache filename based on URL
getCachePath :: String -> FilePath
getCachePath url = cacheDir </> map (\c -> if c == '/' then '_' else c) url

-- Fetch and cache response from PyPI
fetchAndCache :: String -> IO (BL.ByteString, Maybe String)
fetchAndCache url = do
  let cachePath = getCachePath url
  cached <- doesFileExist cachePath
  if cached
    then do
      body <- BL.readFile cachePath
      return (body, Nothing) -- No need to modify cached binary files
    else do
      response <- httpLBS (parseRequest_ url)
      let body = getResponseBody response
          contentType = getResponseHeader "Content-Type" response
      createDirectoryIfMissing True cacheDir
      BL.writeFile cachePath body
      return (body, fmap (BLC.unpack . BLC.fromStrict) (listToMaybe contentType)) -- Convert strict to lazy ByteString

-- Replace PyPI URLs with localhost in HTML responses
rewriteUrls :: BL.ByteString -> BL.ByteString
rewriteUrls content =
  case TLE.decodeUtf8' content of
    Right text -> TLE.encodeUtf8 (TL.replace "https://files.pythonhosted.org" "http://localhost:8080" text)
    Left _ -> content -- If it's binary data, return as is

parseHostWithPort :: Parser (String, Int)
parseHostWithPort = do
    host <- many1 (noneOf ":")
    _ <- char ':'
    port <- many1 digit
    return (host, read port)

-- Sanitize rawPath to handle hostname:port or path
sanitizeURL :: String -> String
sanitizeURL rawPath =
    case parse parseHostWithPort "" rawPath of
        Right (host, port) -> 
            -- Construct URL from host and port
            "https://" ++ host ++ ":" ++ show port
        Left _ -> 
            -- Handle it as a path if parsing fails
            case parseURI rawPath of
                Just uri | uriScheme uri == "https:" ->
                    let path = uriPath uri
                    in "https://pypi.org" ++ path
                _ -> "https://pypi.org" ++ rawPath

-- Main WAI application
app :: Application
app req respond = do
  let rawPath = T.unpack $ TE.decodeUtf8 $ rawPathInfo req
      -- pypiUrl = "https://pypi.org" ++ rawPath
      pypiUrl = sanitizeURL rawPath
  liftIO $ print pypiUrl
  liftIO $ print rawPath
  (body, contentTypeHeader) <- fetchAndCache pypiUrl
  let modifiedBody = rewriteUrls body
      -- If content type is binary or missing, assume HTML (simple pages are HTML)
      contentType = fromMaybe "text/html" contentTypeHeader
  respond $ responseLBS status200 [("Content-Type", TE.encodeUtf8 (T.pack contentType))] modifiedBody

main :: IO ()
main = do
  putStrLn "Running PyPI Reverse Proxy Cache on http://localhost:8080"
  run 8080 app
