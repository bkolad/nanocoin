{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Transaction (
  Transaction(..),
  TransactionHeader(..),

  -- ** Construction
  transferTransaction,

  -- ** Validation
  InvalidTx(..),
  InvalidTxField(..),
  verifyTxSignature,
  validateTransactions,
  applyTransaction,
  applyTransactions,
  invalidTxs,

) where

import Protolude hiding (throwError)

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

data TransactionHeader = Transfer
  { senderKey :: Key.PublicKey
  , recipient :: Address
  , amount    :: Int
  } deriving (Eq, Show)

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

transferTransaction
  :: Key.KeyPair -- ^ Key pair of transfer issuer
  -> Address     -- ^ Address of recipient
  -> Int         -- ^ Transfer amount
  -> IO Transaction
transferTransaction (pubKey, privKey) recipient amnt =
  transaction privKey $ Transfer pubKey recipient amnt

-------------------------------------------------------------------------------
-- Validation & Application of Transactions
-------------------------------------------------------------------------------

data InvalidTxField
  = InvalidTxSignature Text
  | InvalidTransfer Ledger.TransferError
  deriving (Show, Eq, Generic, ToJSON)

data InvalidTx = InvalidTx Transaction InvalidTxField
  deriving (Show, Eq, Generic, ToJSON)

verifyTxSignature
  :: Ledger
  -> Transaction
  -> Either InvalidTx ()
verifyTxSignature l tx = do
  let txHeader = header tx
  case S.decode (signature tx) of
    Left err -> Left $ mkInvalidTx $ InvalidTxSignature (toS err)
    Right sig -> do
      let txHash = hashTxHeader txHeader
      let validSig = Key.verify (senderKey txHeader) sig txHash
      unless validSig $ Left $ mkInvalidTx $
        InvalidTxSignature "Failed to verify transaction signature"
  where
    mkInvalidTx = InvalidTx tx

-- | Validate all transactions with respect to world state
validateTransactions :: Ledger -> [Transaction] -> Either InvalidTx ()
validateTransactions ledger txs =
  case snd (applyTransactions ledger txs) of
    []     -> Right ()
    (x:xs) -> Left x

type ApplyM = State [InvalidTx]

throwError :: InvalidTx -> ApplyM ()
throwError itx = modify (itx:)

runApplyM :: ApplyM a -> (a,[InvalidTx])
runApplyM = flip runState []

-- | Applies a list of transactions to the ledger
applyTransactions
  :: Ledger
  -> [Transaction]
  -> (Ledger,[InvalidTx])
applyTransactions ledger =
  runApplyM . foldM applyTransaction ledger

-- | Applies a transaction to the ledger state
applyTransaction
  :: Ledger
  -> Transaction
  -> ApplyM Ledger
applyTransaction ledger tx@(Transaction hdr sig) = do

  let (Transfer pk to amnt) = hdr

  -- Verify Transaction Signature
  case verifyTxSignature ledger tx of
    Left err -> throwError err
    Right _  -> pure ()

  -- Apply transaction to world state
  let from = deriveAddress pk
  case Ledger.transfer ledger from to amnt of
    Left err -> do
      throwError $ InvalidTx tx $ InvalidTransfer err
      pure ledger
    Right ledger' -> pure ledger'

invalidTxs :: [InvalidTx] -> [Transaction]
invalidTxs itxs = flip map itxs $ \(InvalidTx tx _) -> tx

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance S.Serialize TransactionHeader where
  put (Transfer pk to amnt) = do
    Key.putPublicKey pk
    S.put to
    S.put amnt
  get = Transfer
    <$> Key.getPublicKey
    <*> S.get
    <*> S.get

instance ToJSON TransactionHeader where
  toJSON (Transfer pk to amnt) =
    let (x,y) = Key.extractPoint pk in
    object [ "senderKey" .= object
               [ "x" .= (x :: Integer)
               , "y" .= (y :: Integer)
               ]
           , "recipient" .= toJSON to
           , "amount"    .= toJSON amnt
           ]

instance ToJSON Transaction where
  toJSON (Transaction hdr sig) =
    object [ "header"    .= toJSON hdr
           , "signature" .= Hash.encode64 sig
           ]
