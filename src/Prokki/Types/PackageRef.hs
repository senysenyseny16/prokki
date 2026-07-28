module Prokki.Types.PackageRef (PackageRef (..)) where

import qualified Data.Text as T
import Prokki.Types.Domain (IndexName)

data PackageRef = PackageRef
  { pkgIndexName :: IndexName,
    pkgUrlToken :: T.Text,
    pkgFilename :: T.Text
  }
  deriving (Show, Eq)
