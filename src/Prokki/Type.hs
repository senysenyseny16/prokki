module Prokki.Type (Address (..), Cache (..), Index (..), Indexes, RequestCounters, Path, PackageLinkType (..)) where

import qualified Data.Map as M
import qualified Data.Text as T

data Address = Address {host :: T.Text, port :: Int}

data Index = Index {index :: T.Text, origin :: T.Text, path :: T.Text}

type Indexes = M.Map T.Text Index

type RequestCounters = M.Map T.Text Int

newtype Cache = Cache {cacheDir :: FilePath}

data PackageLinkType = Relative | Absolute

type Path = [T.Text]

instance Show Address where
  show (Address h p) = T.unpack h ++ ":" ++ show p

instance Show Cache where
  show (Cache dir) = "Cache: " ++ dir

instance Show Index where
  show (Index i o p) = "Index: " ++ T.unpack i ++ " -> " ++ T.unpack (o <> p)
