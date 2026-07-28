module Prokki.Types.Domain
  ( IndexName (..),
    Index (..),
    Indexes,
    RequestCounters,
    UploadRegistry,
    ProjectCache,
    PackageCache,
    Sha256 (..),
    Path,
    insertEvicting,
  )
where

import Control.Concurrent.STM (TMVar)
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime)
import Prokki.Types.CacheEntry

data Index = Index
  { indexName :: IndexName,
    indexOrigin :: T.Text,
    indexPath :: T.Text
  }
  deriving (Show, Eq)

newtype IndexName = IndexName {unIndexName :: T.Text} deriving (Show, Eq, Ord)

newtype Sha256 = Sha256 {unSha256 :: T.Text} deriving (Show, Eq, Ord)

type Indexes = M.Map IndexName Index

type RequestCounters = M.Map T.Text Int

type UploadRegistry = M.Map T.Text (TMVar ())

type ProjectCache = PSQ.OrdPSQ (IndexName, T.Text) Int (UTCTime, TL.Text)

type PackageCache = PSQ.OrdPSQ (IndexName, T.Text) Int CacheEntry

type Path = [T.Text]

insertEvicting :: (Ord k, Ord p) => Int -> k -> p -> v -> PSQ.OrdPSQ k p v -> PSQ.OrdPSQ k p v
insertEvicting maxSize key priority val psq =
  let psq' = PSQ.insert key priority val psq
   in if PSQ.size psq' > maxSize
        then maybe psq' (\(_, _, _, rest) -> rest) (PSQ.minView psq')
        else psq'
