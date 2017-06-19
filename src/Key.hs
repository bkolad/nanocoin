{-# LANGUAGE DeriveAnyClass #-}

module Key (
  ECDSA.PublicKey,
  ECDSA.PrivateKey,
  KeyPair,
  newKeyPair,

  ECDSA.sign,
  ECDSA.verify,

  toPublic,
  extractPoint,
  
) where

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA 
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC

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
