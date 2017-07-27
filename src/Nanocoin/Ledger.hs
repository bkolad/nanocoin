{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Ledger (
  Ledger(..)
) where

import Protolude

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Serialize as S 

import Address

-- | Datatype storing the holdings of addresses
newtype Ledger = Ledger
  { unLedger :: Map Address Int }
  deriving (Eq, Show, Generic, S.Serialize)

addAddress :: Ledger -> Address -> Ledger
addAddress ledger addr = Ledger $  
  Map.insert addr 0 $ unLedger ledger
