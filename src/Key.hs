{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Key (
  ECDSA.PublicKey,
  ECDSA.PrivateKey,
  KeyPair(..),
  newKeyPair,

  ECDSA.Signature,
  sign,
  verify,

  toPublic,
  extractPoint,
  mkPublicKey,

  -- ** Node Keys
  readKeys,
  writeKeys,

  -- ** Serialization
  hexPub,
  dehexPub,

  putPublicKey,
  getPublicKey,
  putInteger,
  getInteger,

) where

import Protolude

import qualified Crypto.Number.Basic as CNB
import qualified Crypto.Number.Serialize as CNS
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

import qualified Hash

import System.FilePath  (takeDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist)

-- | All ECC is done using curve SECP256K1
sec_p256k1 :: ECC.Curve
sec_p256k1 = ECC.getCurveByName ECC.SEC_p256k1

type KeyPair = (ECDSA.PublicKey, ECDSA.PrivateKey)

-- | (pk, sk) <- new
-- Returns a new elliptic curve key pair.
--
-- WARNING: Vulnerable to timing attacks.
newKeyPair :: IO KeyPair
newKeyPair = ECC.generate sec_p256k1

-- | Create a public key from a secret key
--
-- WARNING: Vulnerable to timing attacks.
toPublic :: ECDSA.PrivateKey -> ECDSA.PublicKey
toPublic key = ECDSA.PublicKey curve point
  where
    curve  = ECDSA.private_curve key
    curve' = ECC.common_curve curve
    point  = ECC.pointMul curve (ECDSA.private_d key) g
    g      = ECC.ecc_g curve'

-- | UNSAFE: Does not validate (x,y) are valid coordinates to secp256k1
mkPublicKey :: (Integer, Integer) -> ECDSA.PublicKey
mkPublicKey (x,y) = ECDSA.PublicKey sec_p256k1 $ ECC.Point x y

extractPoint :: ECDSA.PublicKey -> (Integer, Integer)
extractPoint pubkey = (x,y)
  where
    ECC.Point x y = ECDSA.public_q pubkey

-- | SHA3_256 hashes a msg before signing
sign :: ECDSA.PrivateKey -> ByteString -> IO ECDSA.Signature
sign pk = ECDSA.sign pk Hash.SHA3_256

-- | Verify a signature of a SHA3_256 encoded ByteString
verify :: ECDSA.PublicKey -> ECDSA.Signature -> ByteString -> Bool
verify = ECDSA.verify Hash.SHA3_256

----------------------------------------------------------------
-- Serialization
----------------------------------------------------------------

-- | Hex encoding of public key
-- XXX Explain encoding
hexPub :: ECDSA.PublicKey -> ByteString
hexPub pubKey = Hash.base16
    (CNS.i2ospOf_ 32 x <> CNS.i2ospOf_ 32 y :: ByteString)
  where
      (x, y) = extractPoint pubKey

-- | Dehex public key
dehexPub :: ByteString -> Either Text ECDSA.PublicKey
dehexPub bs = do
  bs' <- first toS $ BAE.convertFromBase BAE.Base16 bs
  let (xs, ys) = BS.splitAt 32 bs'
  let point = ECC.Point (CNS.os2ip xs) (CNS.os2ip ys)
  if ECC.isPointValid sec_p256k1 point
    then Right $ ECDSA.PublicKey sec_p256k1 point
    else Left "dehexPub: Invalid public key point"

-- | Oh no, Orphan instances!
deriving instance Generic ECDSA.Signature
instance S.Serialize ECDSA.Signature

----------------------------------------------------------------

putPublicKey :: S.Putter ECDSA.PublicKey
putPublicKey pubKey = do
  let (x,y) = Key.extractPoint pubKey
  Key.putInteger x
  Key.putInteger y

-- | UNSAFE: Does not check the validity of the point
getPublicKey :: S.Get ECDSA.PublicKey
getPublicKey = do
  x <- Key.getInteger
  y <- Key.getInteger
  pure $ Key.mkPublicKey (x,y)

encodePublicKey :: ECDSA.PublicKey -> ByteString
encodePublicKey = S.runPut . putPublicKey

decodePublicKey :: ByteString -> Either [Char] ECDSA.PublicKey
decodePublicKey = S.runGet getPublicKey

----------------------------------------------------------------

putPrivateKey :: S.Putter ECDSA.PrivateKey
putPrivateKey = putInteger . ECDSA.private_d

getPrivateKey :: S.Get ECDSA.PrivateKey
getPrivateKey = ECDSA.PrivateKey sec_p256k1 <$> getInteger

encodePrivateKey :: ECDSA.PrivateKey -> ByteString
encodePrivateKey = S.runPut . putPrivateKey

decodePrivateKey :: ByteString -> Either [Char] ECDSA.PrivateKey
decodePrivateKey = S.runGet getPrivateKey

----------------------------------------------------------------

encodeKeyPair :: KeyPair -> ByteString
encodeKeyPair (pubKey, privKey) =
  S.runPut $ do
    putPublicKey pubKey
    putInteger 42
    putPrivateKey privKey

decodeKeyPair :: ByteString -> Either [Char] KeyPair
decodeKeyPair bs =
  flip S.runGet bs $ do
    pubKey <- getPublicKey
    getInteger
    privKey <- getPrivateKey
    pure $ (pubKey,privKey)

----------------------------------------------------------------

-- | Serialize an Integer
putInteger :: S.Putter Integer
putInteger n = do
  let nBytes = CNB.numBytes n
  S.putInt64le $ fromIntegral nBytes
  S.putByteString $ CNS.i2ospOf_ nBytes n

-- | Deserialize an Integer
-- UNSAFE: vulnerable to long Integer injection attacks
getInteger :: S.Get Integer
getInteger = do
  nBytes <- fromIntegral <$> S.getInt64le
  CNS.os2ip <$> S.getByteString nBytes

encodeInteger :: Integer -> ByteString
encodeInteger = S.runPut . putInteger

----------------------------------------------------------------
-- Node Keys
----------------------------------------------------------------

writeKeys (Just path) keys = do
  mkKeysDir $ takeDirectory path
  BS.writeFile path $ encodeKeyPair keys

readKeys :: FilePath -> IO (Either [Char] KeyPair)
readKeys fp = do
  fileExists <- doesFileExist fp
  if fileExists
    then do
      keysBS <- BS.readFile fp
      case decodeKeyPair keysBS of
        Left err -> pure $ Left $ toS err
        Right kp -> pure $ Right kp
    else pure $
      Left "Supplied keys directory invalid"

mkKeysDir :: FilePath -> IO ()
mkKeysDir = createDirectoryIfMissing False
