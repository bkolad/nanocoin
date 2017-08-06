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

import Address (Address, rawAddress, deriveAddress)
import Hash (Hash)
import qualified Hash 
import qualified Key 

import Nanocoin.Ledger (Ledger)

type Timestamp = Integer

data Transfer = Transfer
  { from   :: Address
  , to     :: Address
  , amount :: Int
  } deriving (Eq, Show, Generic, S.Serialize)

newtype NewAccount = NewAccount 
  { publicKey :: Key.PublicKey
  } deriving (Eq, Show, Generic)

instance S.Serialize NewAccount where
  put (NewAccount pubKey) = do
    let (r,s) = Key.extractPoint pubKey
    Key.putInteger r
    Key.putInteger s

  get = do
    r <- Key.getInteger
    s <- Key.getInteger
    pure $ NewAccount $ Key.mkPublicKey (r,s)


data TransactionHeader 
  = TxTransfer Transfer
  | TxAccount NewAccount 
  deriving (Eq, Show, Generic, S.Serialize)

data Transaction
  = Transaction 
      { header    :: TransactionHeader
      , sender    :: Address 
      , signature :: ByteString
      , timestamp :: Timestamp
      }
  deriving (Eq, Show, Generic, S.Serialize)

hashTransaction :: Transaction -> ByteString
hashTransaction Transaction{..} = Hash.getHash $ Hash.sha256 $ BS.concat
    [ hashTxHeader header, S.encode sender, B8.pack (show timestamp) ]
  where
    hashTxHeader = Hash.getHash . Hash.sha256 . S.encode

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Transfer where
  toJSON (Transfer from to amnt) = 
    object [ "fromAddress" .= Hash.encode64 (S.encode from)
           , "toAddress"   .= Hash.encode64 (S.encode to)
           , "amount"      .= toJSON amnt
           ]

instance FromJSON Transfer where
  parseJSON (Object o) = do
    fromAddr' <- fmap S.decode $ Hash.decode64 =<< (o .: "fromAddress") 
    toAddr'   <- fmap S.decode $ Hash.decode64 =<< (o .: "toAddress") 
    case (,) <$> fromAddr' <*> toAddr' of
      Left err -> typeMismatch "TransactionHeader: address" (Object o) 
      Right (fromAddr, toAddr) -> do
        amnt <- o .: "amount" 
        pure $ Transfer fromAddr toAddr amnt
  parseJSON invalid = typeMismatch "TransactionHeader" invalid

instance ToJSON NewAccount where
  toJSON (NewAccount pubKey) = 
    object [ "pubKey " .= Hash.encode64 (Key.hexPub pubKey) 
           ]

instance FromJSON NewAccount where
  parseJSON (Object o) = do
    ePubKey <- fmap Key.dehexPub $ Hash.decode64 =<< (o .: "pubKey")
    case ePubKey of
      Left err -> typeMismatch "NewAccount: pubKey" (Object o)
      Right pubKey -> pure $ NewAccount pubKey

instance ToJSON TransactionHeader 
instance FromJSON TransactionHeader 

instance ToJSON Transaction where
  toJSON (Transaction h i s t) = 
    object [ "header"    .= h
           , "issuer"    .= i
           , "signature" .= Hash.encode64 s
           , "timestamp" .= t
           ]

instance FromJSON Transaction where
  parseJSON (Object o) =
    Transaction <$>  o .: "header"
                <*>  o .: "issuer"
                <*> (o .: "signature" >>= Hash.decode64)
                <*>  o .: "timestamp"
  parseJSON invalid = typeMismatch "Transaction" invalid
    
