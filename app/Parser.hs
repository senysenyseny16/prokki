{-# LANGUAGE OverloadedStrings #-}

module Parser (settingsParser) where

import qualified Data.Text as T
import Options.Applicative
import Prokki.Settings (Address (..), Cache (..), Index (..), Settings (..))

settingsParser :: Parser Settings
settingsParser = Settings <$> addressParser <*> indexParser <*> cacheParser

addressParser :: Parser Address
addressParser =
  Address
    <$> (T.pack <$> strOption (long "host" <> metavar "HOST" <> help "Host address" <> value "0.0.0.0" <> showDefault))
    <*> option auto (long "port" <> metavar "PORT" <> help "Port number" <> value 8080 <> showDefault)

indexParser :: Parser Index
indexParser =
  Index . T.pack
    <$> strOption
      (long "index-url" <> metavar "INDEX_URL" <> help "Python Index (URL) to cache" <> value "https://pypi.org/" <> showDefault)

cacheParser :: Parser Cache
cacheParser =
  Cache
    <$> strOption
      (long "cache-dir" <> metavar "CACHE_DIR" <> help "Directory to store cache" <> value "index-cache" <> showDefault)
