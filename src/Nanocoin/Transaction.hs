{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards#-}

module Nanocoin.Transaction (
  Transaction(..)
) where

import Protolude

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Serialize as S

import Address (Address, rawAddress)
import Hash (Hash)
import qualified Hash 
import qualified Key 

import Nanocoin.Ledger (Ledger)

type Timestamp = Integer

data TransactionHeader 
  = Transfer
      { from   :: Address
      , to     :: Address
      , amount :: Int
      }
  deriving (Eq, Show, Generic, S.Serialize)

data Transaction
  = Transaction 
      { header    :: TransactionHeader
      , signature :: ByteString
      , timestamp :: Timestamp
      }
  deriving (Eq, Show, Generic, S.Serialize)

hashTransaction :: Transaction -> ByteString
hashTransaction Transaction{..} = Hash.getHash $ Hash.sha256 $ BS.concat
    [ headerHash header, B8.pack (show timestamp) ]
  where
    headerHash Transfer{..} = 
      Hash.getHash $ Hash.sha256 $ BS.concat
        [ rawAddress from, rawAddress to, B8.pack (show amount) ]

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON TransactionHeader where
  toJSON (Transfer from to amnt) = 
    object [ "fromAddress" .= Hash.encode64 (S.encode from)
           , "toAddress"   .= Hash.encode64 (S.encode to)
           , "amount"      .= toJSON amnt
           ]

instance FromJSON TransactionHeader where
  parseJSON (Object o) = do
    fromAddr' <- fmap S.decode $ Hash.decode64 =<< (o .: "fromAddress") 
    toAddr'   <- fmap S.decode $ Hash.decode64 =<< (o .: "toAddress") 
    case (,) <$> fromAddr' <*> toAddr' of
      Left err -> typeMismatch "TransactionHeader: address" (Object o) 
      Right (fromAddr, toAddr) -> do
        amnt <- o .: "amount" 
        pure $ Transfer fromAddr toAddr amnt
  parseJSON invalid = typeMismatch "TransactionHeader" invalid

instance ToJSON Transaction where
  toJSON (Transaction h s t) = 
    object [ "header"    .= toJSON h
           , "signature" .= Hash.encode64 s
           , "timestamp" .= toJSON t
           ]

instance FromJSON Transaction where
  parseJSON (Object o) =
    Transaction <$>  o .: "header"
                <*> (o .: "signature" >>= Hash.decode64)
                <*>  o .: "timestamp"
  parseJSON invalid = typeMismatch "Transaction" invalid
    
