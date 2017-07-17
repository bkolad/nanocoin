{-# Language NoImplicitPrelude #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Hash (
  Hash,
  rawHash,
  getHash,
  sha256,
  sha256Raw,
  sha256Raw',
  ripemd160,
  ripemd160Raw,

  SHA3_256(..),

  validateSha,
  validateSha',
  validateSha16,

  base16,
  unbase16
) where

import Protolude

import Crypto.Hash
import Crypto.Hash.Algorithms

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA

-- | Base16 encoding
hashSize = 64

newtype Hash a = Hash { rawHash :: Digest SHA3_256 }
  deriving (Eq, Ord, BA.ByteArrayAccess)

getHash :: Hash a -> ByteString
getHash = base16 . rawHash 

-- | Compute SHA-256 hash of a bytestring.
-- Maximum input size is (2^{64}-1)/8 bytes.
--
-- > Output size         : 256
-- > Internal state size : 1600
-- > Block size          : 1088
-- > Length size         : n/a
-- > Word size           : 64
-- > Rounds              : 24
sha256Raw :: ByteString -> Digest SHA3_256 
sha256Raw x = hash x :: Digest SHA3_256

sha256Raw' :: ByteString -> ByteString
sha256Raw' = BA.convert . sha256Raw

-- | Base16 encoded
sha256 :: ByteString -> Hash a 
sha256 = Hash . sha256Raw

-- | Compute RIPEMD-160 hash of a bytestring.
--
-- > Output size         : 160
-- > Internal state size : 128
-- > Block size          : 512
-- > Length size         : 64
-- > Word size           : 32
-- > Rounds              : 80
ripemd160Raw :: ByteString -> ByteString 
ripemd160Raw x = BA.convert (hash x :: Digest RIPEMD160)

-- | Base16 encoded, 64 bits 
ripemd160 :: ByteString -> ByteString
ripemd160 = base16 . ripemd160Raw

validateSha :: Hash a -> Bool
validateSha h = BA.length (rawHash h) == hashSize

validateSha' :: ByteString -> Bool
validateSha' bs = BA.length bs == hashSize

validateSha16 :: ByteString -> Bool
validateSha16 bs = case unbase16 bs of
  Left err -> False
  Right bs' -> BA.length (bs' :: ByteString) == hashSize

base16 :: (BA.ByteArrayAccess bin, BA.ByteArray bout) => bin -> bout 
base16 = BA.convertToBase BA.Base16

unbase16 :: (BA.ByteArrayAccess bin, BA.ByteArray bout) => bin -> Either [Char] bout 
unbase16 = BA.convertFromBase BA.Base16 

