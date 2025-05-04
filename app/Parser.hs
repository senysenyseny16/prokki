module Parser (argsParser, Args (..)) where

import Options.Applicative

newtype Args = Args {configPath :: FilePath}

argsParser :: Parser Args
argsParser = Args <$> strOption (long "config" <> metavar "CONFIG" <> help "Configuration file path")
