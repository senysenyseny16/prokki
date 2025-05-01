module Prokki.Type (Address (..), Index (..), Cache (..)) where

import qualified Data.Text as T

data Address = Address {host :: T.Text, port :: Int}

newtype Index = Index {indexUrl :: T.Text}

newtype Cache = Cache {cacheDir :: FilePath}

instance Show Address where
  show (Address h p) = T.unpack h ++ ":" ++ show p

instance Show Index where
  show (Index url) = "Index: " ++ T.unpack url

instance Show Cache where
  show (Cache dir) = "Cache: " ++ dir
