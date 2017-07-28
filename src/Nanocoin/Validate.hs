

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
validateBlock = undefined

validateTransaction 
  :: Ledger 
  -> Transaction 
  -> Either InvalidTransaction ()
validateTransaction = undefined

verifyBlockSignature 
  :: Ledger 
  -> Block 
  -> Either InvalidBlock ()
verifyBlockSignature = undefined

verifyTransactionSignature
  :: Ledger
  -> Block
  -> Either InvalidTransaction ()
verifyTransactionSignature = undefined
