{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Transaction (
  Transaction(..),
  TransactionHeader(..),
  Transfer(..),
  CreateAccount(..),
 
  -- ** Validation
  InvalidTransaction(..),
  validateTransaction

) where

import Protolude

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Serialize as S

import Address (Address, rawAddress, deriveAddress)
import Hash (Hash)
import Nanocoin.Ledger (Ledger)

import qualified Hash 
import qualified Key 
import qualified Nanocoin.Ledger as Ledger


type Timestamp = Integer

data Transfer = Transfer
  { issuer    :: Address
  , recipient :: Address
  , amount    :: Int
  } deriving (Eq, Show, Generic, S.Serialize)

newtype CreateAccount = CreateAccount 
  { publicKey :: Key.PublicKey
  } deriving (Eq, Show, Generic)

instance S.Serialize CreateAccount where
  put (CreateAccount pubKey) = Key.putPublicKey pubKey 
  get = CreateAccount <$> Key.getPublicKey 

data TransactionHeader 
  = TxTransfer Transfer
  | TxAccount CreateAccount 
  deriving (Eq, Show, Generic, S.Serialize)

data Transaction = Transaction 
  { header    :: TransactionHeader
  , signature :: ByteString
  } deriving (Eq, Show, Generic, S.Serialize)

hashTransaction :: Transaction -> ByteString
hashTransaction = Hash.getHash . Hash.sha256 . hashTxHeader . header
 
hashTxHeader :: TransactionHeader -> ByteString
hashTxHeader = Hash.getHash . Hash.sha256 . S.encode

-------------------------------------------------------------------------------
-- Transaction Construction
-------------------------------------------------------------------------------

transaction 
  :: Key.PrivateKey 
  -> TransactionHeader 
  -> IO Transaction
transaction privKey txHdr = do
  txSig <- Key.sign privKey $ hashTxHeader txHdr
  pure $ Transaction txHdr $ S.encode txSig

addAccountTx 
  :: Key.KeyPair
  -> IO Transaction
addAccountTx (Key.KeyPair pubKey privKey) = 
  transaction privKey (TxAccount $ CreateAccount pubKey)
  
transferTx 
  :: Key.KeyPair -- ^ Key pair of transfer issuer
  -> Address     -- ^ Address of recipient
  -> Int         -- ^ Transfer amount
  -> IO Transaction
transferTx (Key.KeyPair pubKey privKey) recipient amnt = 
    transaction privKey (TxTransfer transfer') 
  where
    transfer' = Transfer issuer recipient amnt 
    issuer = Address.deriveAddress pubKey

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data InvalidTransaction
  = InvalidTxSignature Text 
  | InvalidTransfer Ledger.TransferError
  | InvalidAccount Ledger.AddAccountError
  | InvalidTranferIssuer Address

verifyTxSignature 
  :: Ledger
  -> Transaction 
  -> Either InvalidTransaction () 
verifyTxSignature l tx = do 
  let txHeader = header tx 
  pubKey <- case txHeader of
    TxAccount (CreateAccount pubKey) -> Right pubKey 
    TxTransfer (Transfer issuer _ _) -> 
      case Ledger.lookupAccount issuer l of
        Nothing -> Left $ InvalidTranferIssuer issuer
        Just acc -> Right $ fst acc
  case S.decode (signature tx) of
    Left err -> Left $ InvalidTxSignature (toS err)
    Right sig -> do
      let validSig = Key.verify pubKey sig (S.encode txHeader)
      unless validSig $
        Left $ InvalidTxSignature "Failed to verify transaction signature"

validateTransaction 
  :: Ledger 
  -> Transaction 
  -> Either InvalidTransaction ()
validateTransaction _ _ = Right () -- XXX 

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Transfer where
  toJSON (Transfer from to amnt) = 
    object [ "issuer"    .= Hash.encode64 (S.encode from)
           , "recipient" .= Hash.encode64 (S.encode to)
           , "amount"    .= toJSON amnt
           ]

instance FromJSON Transfer where
  parseJSON (Object o) = do
    fromAddr' <- fmap S.decode $ Hash.decode64 =<< (o .: "issuer") 
    toAddr'   <- fmap S.decode $ Hash.decode64 =<< (o .: "recipient") 
    case (,) <$> fromAddr' <*> toAddr' of
      Left err -> typeMismatch "TransactionHeader: address" (Object o) 
      Right (fromAddr, toAddr) -> do
        amnt <- o .: "amount" 
        pure $ Transfer fromAddr toAddr amnt
  parseJSON invalid = typeMismatch "TransactionHeader" invalid

instance ToJSON CreateAccount where
  toJSON (CreateAccount pubKey) = 
    object [ "pubKey " .= Hash.encode64 (Key.hexPub pubKey) 
           ]

instance FromJSON CreateAccount where
  parseJSON (Object o) = do
    ePubKey <- fmap Key.dehexPub $ Hash.decode64 =<< (o .: "pubKey")
    case ePubKey of
      Left err -> typeMismatch "CreateAccount: pubKey" (Object o)
      Right pubKey -> pure $ CreateAccount pubKey

instance ToJSON TransactionHeader 
instance FromJSON TransactionHeader 

instance ToJSON Transaction where
  toJSON (Transaction h s) = 
    object [ "header"    .= h
           , "signature" .= Hash.encode64 s
           ]

instance FromJSON Transaction where
  parseJSON (Object o) =
    Transaction <$>  o .: "header"
                <*> (o .: "signature" >>= Hash.decode64)
  parseJSON invalid = typeMismatch "Transaction" invalid
    
