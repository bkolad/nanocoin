{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Ledger (
  Ledger,
  addAddress,
  updateBalance,
) where

import Protolude

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Serialize as S 

import Address

type Balance = Int

data TransferError 
  = InvalidSender Address
  | InvalidReceiver Address
  | InsufficientBalance Address Balance 
  deriving (Eq, Show) 

-- | Datatype storing the holdings of addresses
newtype Ledger = Ledger
  { unLedger :: Map Address Balance 
  } deriving (Eq, Show, Generic, S.Serialize, Monoid)

emptyLedger :: Ledger
emptyLedger = Ledger mempty

type Transition = Ledger -> Ledger

addAddress :: Address -> Transition
addAddress addr = updateBalance addr 0

lookupBalance :: Address -> Ledger -> Maybe Balance
lookupBalance addr = Map.lookup addr . unLedger 

updateBalance :: Address -> Int -> Transition
updateBalance addr amount = 
  Ledger . Map.insert addr amount . unLedger 

transfer 
  :: Ledger 
  -> Address
  -> Address
  -> Balance
  -> Either TransferError Ledger
transfer ledger fromAddr toAddr amount = do
  senderBal <-
    case lookupBalance fromAddr ledger of
      Nothing -> Left $ InvalidSender fromAddr
      Just bal -> Right bal 
 
  recvrBal <- 
    case lookupBalance fromAddr ledger of
      Nothing -> Left $ InvalidReceiver toAddr
      Just bal -> Right bal 

  if senderBal < amount 
    then Left $ InsufficientBalance fromAddr senderBal
    else do
      let newSenderBal = senderBal - amount
      let newRecvrBal = recvrBal + amount
      Right $ updateBalance toAddr newRecvrBal $ 
        updateBalance fromAddr newSenderBal ledger
