module Prokki.Types.Network (Address (..)) where

import qualified Data.Text as T

data Address = Address {addressHost :: T.Text, addressPort :: Int} deriving (Show, Eq)
