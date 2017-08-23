{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanocoin.MemPool (
  MemPool(..),
  addTransaction,
  removeTransactions,
) where

import Protolude

import Data.Aeson
import Data.List ((\\))

import Nanocoin.Transaction (Transaction)

-- XXX [Transaction] to DList Transaction

newtype MemPool = MemPool
  { unMemPool :: [Transaction]
  } deriving (Show, Eq, Generic, Monoid, ToJSON)

addTransaction
  :: Transaction
  -> MemPool
  -> MemPool
addTransaction tx (MemPool pool) =
  MemPool (pool ++ [tx])

removeTransactions
  :: [Transaction]
  -> MemPool
  -> MemPool
removeTransactions txs (MemPool pool) =
  MemPool $ pool \\ txs
