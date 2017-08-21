{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanocoin.Ledger (
  Ledger,

  transfer,
  TransferError,
) where

import Protolude

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Serialize as S

import Address
import qualified Key

type Balance = Int

data TransferError
  = AddressDoesNotExist Address
  | InvalidTransferAmount Int
  | InsufficientBalance Address Balance
  deriving (Eq, Show, Generic)

instance ToJSON TransferError

-- | Datatype storing the holdings of addresses
newtype Ledger = Ledger
  { unLedger :: Map Address Balance
  } deriving (Eq, Show, Generic, Monoid)

instance ToJSON Ledger where
  toJSON = toJSON . unLedger

emptyLedger :: Ledger
emptyLedger = Ledger mempty

lookupBalance :: Address -> Ledger -> Maybe Balance
lookupBalance addr = Map.lookup addr . unLedger

-- | Add an integer to an account's existing balance
addBalance :: Address -> Int -> Ledger -> Ledger
addBalance addr amount =
  Ledger . Map.adjust (+amount) addr . unLedger

-- | Add an address with 1000 balance to the Ledger
addAddress :: Address -> Ledger -> Ledger
addAddress addr ledger =
    case lookupBalance addr ledger of
      Nothing -> Ledger $ Map.insert addr 1000 ledger'
      Just _  -> ledger
  where
    ledger' = unLedger ledger

-- | Transfer Nanocoin from one account to another
transfer
  :: Ledger
  -> Address
  -> Address
  -> Balance
  -> Either TransferError Ledger
transfer ledger fromAddr toAddr amount = do

  let ledger' = addAddress fromAddr (addAddress toAddr ledger)

  senderBal <-
    case lookupBalance fromAddr ledger of
      Nothing  -> Left $ AddressDoesNotExist fromAddr
      Just bal -> Right bal

  recvrBal <-
    case lookupBalance toAddr ledger of
      Nothing  -> Left $ AddressDoesNotExist toAddr
      Just bal -> Right bal

  when (amount < 1) $
    Left $ InvalidTransferAmount amount

  when (senderBal < amount) $
    Left $ InsufficientBalance fromAddr senderBal

  Right $ addBalance toAddr amount
    $ addBalance fromAddr (-amount) ledger'
