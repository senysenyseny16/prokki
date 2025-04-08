{-# LANGUAGE OverloadedStrings #-}

module Prokki.Utils (replaceSubstring, compress) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

replaceSubstring :: LBS.ByteString -> LBS.ByteString
replaceSubstring body =
  let decoded = (decodeUtf8 . LBS8.toStrict) body
      replaced = T.replace "https://files.pythonhosted.org" "http://localhost:8080" decoded
   in (LBS.fromStrict . encodeUtf8) replaced

compress :: LBS8.ByteString -> LBS8.ByteString
compress = GZip.compressWith GZip.defaultCompressParams {GZip.compressLevel = GZip.bestCompression}
