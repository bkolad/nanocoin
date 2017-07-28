

module Nanocoin.MemPool (
  MemPool(..)
) where

import Protolude

import Nanocoin.Transaction (Transaction)

newtype MemPool = MemPool 
  { unMemPool :: [Transaction] }

addTransaction 
  :: MemPool 
  -> Transaction 
  -> MemPool
addTransaction pool tx = undefined

isTransactionUnique :: Transaction -> MemPool -> Bool
isTransactionUnique = undefined 
