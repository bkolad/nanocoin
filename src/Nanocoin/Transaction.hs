{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Transaction (
  Transaction(..),
  TransactionHeader(..),
  Transfer(..),
  CreateAccount(..),

  -- ** Construction
  addAccountTx,
  transferTx,

  -- ** Validation
  InvalidTransaction(..),
  verifyTxSignature,
  applyTransaction,
  applyTransactions

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

data Transfer = Transfer
  { issuer    :: Address
  , recipient :: Address
  , amount    :: Int
  } deriving (Eq, Show, Generic, S.Serialize, ToJSON)

newtype CreateAccount = CreateAccount
  { publicKey :: Key.PublicKey
  } deriving (Eq, Show, Generic)

data TransactionHeader
  = TxTransfer Transfer
  | TxAccount CreateAccount
  deriving (Eq, Show, Generic, S.Serialize, ToJSON)

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
addAccountTx (pubKey, privKey) =
  transaction privKey (TxAccount $ CreateAccount pubKey)

transferTx
  :: Key.KeyPair -- ^ Key pair of transfer issuer
  -> Address     -- ^ Address of recipient
  -> Int         -- ^ Transfer amount
  -> IO Transaction
transferTx (pubKey, privKey) recipient amnt =
    transaction privKey (TxTransfer transfer')
  where
    transfer' = Transfer issuer recipient amnt
    issuer = Address.deriveAddress pubKey

-------------------------------------------------------------------------------
-- Validation & Application of Transactions
-------------------------------------------------------------------------------

data InvalidTransaction
  = InvalidTxSignature Text
  | InvalidTransfer Ledger.TransferError
  | InvalidAccount Ledger.AddAccountError
  | InvalidTranferIssuer Address
  deriving (Show, Eq)

verifyTxSignature
  :: Ledger
  -> Transaction
  -> Either InvalidTransaction ()
verifyTxSignature l tx = do
  let txHeader = header tx
  pubKey <- case txHeader of
    -- Create account transactions are self-signed
    TxAccount (CreateAccount pubKey) -> Right pubKey
    -- To transfer, the tx hash must be signed by the issuer
    TxTransfer (Transfer issuer _ _) ->
      case Ledger.lookupAccount issuer l of
        Nothing -> Left $ InvalidTranferIssuer issuer
        Just acc -> Right $ fst acc
  case S.decode (signature tx) of
    Left err -> Left $ InvalidTxSignature (toS err)
    Right sig -> do
      let txHash = hashTxHeader txHeader
      let validSig = Key.verify pubKey sig txHash
      unless validSig $
        Left $ InvalidTxSignature "Failed to verify transaction signature"

type ApplyM = State [InvalidTransaction]

throwError :: InvalidTransaction -> ApplyM ()
throwError itx = modify (itx:)

runApplyM :: ApplyM a -> (a,[InvalidTransaction])
runApplyM = flip runState []

-- | Applies a list of transactions to the ledger
applyTransactions
  :: Ledger
  -> [Transaction]
  -> (Ledger,[InvalidTransaction])
applyTransactions ledger =
  runApplyM . foldM applyTransaction ledger

-- | Applies a transaction to the ledger state
applyTransaction
  :: Ledger
  -> Transaction
  -> ApplyM Ledger
applyTransaction ledger tx = do
  -- Verify Transaction Signature
  case verifyTxSignature ledger tx of
    Left err -> throwError err
    Right _  -> pure ()
  -- Apply transaction to world state
  case header tx of
    TxAccount (CreateAccount pubkey) ->
      case Ledger.addAccount pubkey ledger of
        Left err -> do
          throwError $ InvalidAccount err
          pure ledger
        Right ledger' -> pure ledger'
    TxTransfer (Transfer from to amnt) ->
      case Ledger.transfer ledger from to amnt of
        Left err -> do
          throwError $ InvalidTransfer err
          pure ledger
        Right ledger' -> pure ledger'

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON CreateAccount where
  toJSON (CreateAccount pubKey) =
    let (x,y) = Key.extractPoint pubKey in
    object [ "tag" .= ("CreateAccount" :: Text)
           , "contents" .=
               object [ "x" .= (x :: Integer)
                      , "y" .= (y :: Integer)
                      ]
           ]

instance S.Serialize CreateAccount where
  put (CreateAccount pubKey) = Key.putPublicKey pubKey
  get = CreateAccount <$> Key.getPublicKey

instance ToJSON Transaction where
  toJSON (Transaction h s) =
    object [ "header"    .= h
           , "signature" .= Hash.encode64 s
           ]
