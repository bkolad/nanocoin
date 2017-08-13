{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanocoin.MemPool (
  MemPool(..)
) where

import Protolude

import Data.Aeson

import Nanocoin.Transaction (Transaction)

-- XXX [Transaction] to DList Transaction

newtype MemPool = MemPool 
  { unMemPool :: [Transaction] 
  } deriving (Show, Eq, Generic, Monoid, ToJSON)

addTransaction 
  :: Transaction 
  -> MemPool 
  -> MemPool
addTransaction tx (MemPool pool) = MemPool (pool ++ [tx])
  
