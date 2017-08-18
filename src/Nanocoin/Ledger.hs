{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanocoin.Ledger (
  Ledger,

  addAccount,
  AddAccountError,
  lookupAccount,

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

type Account = (Key.PublicKey, Balance)

data TransferError
  = InvalidSender Address
  | InvalidReceiver Address
  | InsufficientBalance Address Balance
  deriving (Eq, Show)

data AddAccountError = AccountExists Address
  deriving (Eq, Show)

-- | Datatype storing the holdings of addresses
newtype Ledger = Ledger
  { unLedger :: Map Address Account
  } deriving (Eq, Show, Generic, Monoid)

instance ToJSON Ledger where
  toJSON = toJSON . map snd . unLedger

emptyLedger :: Ledger
emptyLedger = Ledger mempty

lookupAccount :: Address -> Ledger -> Maybe Account
lookupAccount addr = Map.lookup addr . unLedger

-- | Add an integer to an account's existing balance
addBalance :: Address -> Int -> Ledger -> Ledger
addBalance addr amount =
    Ledger . Map.adjust addBalance' addr . unLedger
  where
    addBalance' = second (+amount)

-- | Add an account with 0 balance to the Ledger
addAccount :: Key.PublicKey -> Ledger -> Either AddAccountError Ledger
addAccount pubKey ledger =
    case lookupAccount newAddr ledger of
      Nothing -> Right $ Ledger $ Map.insert newAddr newAcc ledger'
      Just _  -> Left $ AccountExists newAddr
  where
    ledger' = unLedger ledger

    newAddr = Address.deriveAddress pubKey

    -- Automatically give every new account 1000 Nanocoins because yes
    newAcc  = (pubKey, 1000)

-- | Transfer Nanocoin from one account to another
transfer
  :: Ledger
  -> Address
  -> Address
  -> Balance
  -> Either TransferError Ledger
transfer ledger fromAddr toAddr amount = do
  senderBal <-
    case lookupAccount fromAddr ledger of
      Nothing -> Left $ InvalidSender fromAddr
      Just acc -> Right $ snd acc

  recvrBal <-
    case lookupAccount fromAddr ledger of
      Nothing -> Left $ InvalidReceiver toAddr
      Just acc -> Right $ snd acc

  if senderBal < amount
    then Left $ InsufficientBalance fromAddr senderBal
    else Right $
      addBalance toAddr amount $
        addBalance fromAddr (-amount) ledger
