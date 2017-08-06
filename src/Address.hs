{-# Language NoImplicitPrelude #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Address (
  Address,
  rawAddress,
  mkAddress,
  deriveAddress,
) where

import Protolude

import Crypto.Number.Serialize (i2osp)

import Data.Aeson 
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base58 as B58
import qualified Data.Serialize as S

import qualified Hash
import qualified Key 

addrSize :: Int
addrSize = 32

-- | A ledger address, derived from elliptic curve point
newtype Address = Address { rawAddress :: ByteString } 
  deriving (Show, Eq, Ord, Monoid, S.Serialize, IsString)

instance ToJSON Address where
  toJSON (Address bs) = Data.Aeson.String (decodeUtf8 bs)

-- XXX UNSAFE decode
instance FromJSON Address where
  parseJSON (String s) = pure $ Address $ encodeUtf8 s

-- | UNSAFE: Does not validate ByteString supplied
mkAddress :: ByteString -> Address
mkAddress = Address . b58

-- | Derive an address from an ECC public key
--
-- > address(x,y) = addrHash(string(x) <> string(y))
deriveAddress :: Key.PublicKey -> Address
deriveAddress pub = Address (b58 addr)
  where
    (x, y) = Key.extractPoint pub
    addr   = BA.convert $ deriveHash pstr
    pstr   = (i2osp x) <> ";" <> (i2osp y)

-- | Address derivation function, maps a hash of a EC point to a unique,
-- irreversible identity that uniquely defines a participant in the network and
-- any participant can verify integrity of it's coherence to a public key.
--
-- > addrHash(n) = sha256(sha256(ripemd160(sha256(n))))
deriveHash :: ByteString -> ByteString
deriveHash pstr = Hash.sha256Raw'
                $ Hash.sha256Raw' 
                $ Hash.ripemd160Raw 
                $ Hash.sha256Raw' pstr

-- | Validate whether an address is a well-formed B58 encoded hash.
validateAddress :: Address -> Bool
validateAddress (Address addr) = 
  case unb58 addr of
    Nothing  -> False
    Just sha -> Hash.validateSha' sha
  
-------------------------------------------------------------------------------
-- Base58 Encoding
-------------------------------------------------------------------------------

b58 :: ByteString -> ByteString
b58 = B58.encodeBase58 B58.bitcoinAlphabet

unb58 :: ByteString -> Maybe ByteString
unb58 = B58.decodeBase58 B58.bitcoinAlphabet

b58hash :: ByteString -> ByteString
b58hash = b58 . Hash.sha256Raw'

