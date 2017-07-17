{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Key (
  ECDSA.PublicKey,
  ECDSA.PrivateKey,
  KeyPair(..),
  newKeyPair,

  ECDSA.Signature,
  ECDSA.sign,
  signS,
  ECDSA.verify,

  toPublic,
  extractPoint,
  
) where

import Protolude

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA 
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC

import qualified Data.Serialize as S

import qualified Hash

-- | Oh no, Orphan instance! Fuck it...
deriving instance Generic ECDSA.Signature
instance S.Serialize ECDSA.Signature

-- | All ECC is done using curve SECP256K1
sec_p256k1 :: ECC.Curve
sec_p256k1 = ECC.getCurveByName ECC.SEC_p256k1

data KeyPair = KeyPair 
  { publicKey  :: ECDSA.PublicKey
  , privateKey :: ECDSA.PrivateKey
  }

getKeyPair :: KeyPair -> (ECDSA.PublicKey, ECDSA.PrivateKey)
getKeyPair (KeyPair pk sk) = (pk, sk)

-- | (pk, sk) <- new
-- Returns a new elliptic curve key pair.
--
-- WARNING: Vulnerable to timing attacks.
newKeyPair :: IO KeyPair
newKeyPair = do
  (pubKey, privKey) <- ECC.generate sec_p256k1
  return $ KeyPair pubKey privKey

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

extractPoint :: ECDSA.PublicKey -> (Integer, Integer)
extractPoint pubkey = (x,y)
  where
    ECC.Point x y = ECDSA.public_q pubkey 

-- | Serializes a msg before signing
signS :: S.Serialize a => ECDSA.PrivateKey -> a -> IO ECDSA.Signature
signS pk msg = ECDSA.sign pk Hash.SHA3_256 (S.encode msg)
