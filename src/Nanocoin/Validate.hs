

module Nanocoin.Validate (

) where

import Protolude

import qualified Key

import Nanocoin.Block
import Nanocoin.Ledger (Ledger)
import Nanocoin.Transaction

import qualified Nanocoin.Ledger as Ledger

data InvalidBlock 
  = InvalidBlockSignature
  | InvalidBlockIndex 
  | InvalidBlockHash
  | InvalidBlockTx InvalidTransaction

data InvalidTransaction
  = InvalidTxSignature Key.Signature 
  | InvalidTransfer Ledger.TransferError
  | InvalidAccount Ledger.AddAccountError

validateBlock 
  :: Ledger 
  -> Block 
  -> Either InvalidBlock ()
validateBlock _ _ = Right () -- XXX

validateTransaction 
  :: Ledger 
  -> Transaction 
  -> Either InvalidTransaction ()
validateTransaction _ _ = Right () -- XXX 

verifyBlockSignature 
  :: Ledger 
  -> Block 
  -> Either InvalidBlock ()
verifyBlockSignature _ _ = Right () -- XXX 

verifyTransactionSignature
  :: Ledger
  -> Block
  -> Either InvalidTransaction ()
verifyTransactionSignature _ _ = Right () -- XXX 
