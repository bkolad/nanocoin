

module Nanocoin.Validate (

) where

import Protolude

import Key

import Nanocoin.Block
import Nanocoin.Ledger
import Nanocoin.Transaction

data InvalidBlock 
  = InvalidBlockSignature
  | InvalidBlockIndex 
  | InvalidBlockHash
  | InvalidBlockTx InvalidTransaction

data InvalidTransaction
  = InvalidTxSignature
  | InvalidTxTimestamp
  | InvalidTxTransfer

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
