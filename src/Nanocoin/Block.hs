{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin.Block (
  Block(..),
  BlockHeader(..),
  Blockchain,
  hashBlock,
  hashBlockHeader,
  genesisBlock,

  proofOfWork,
  mineBlock,
  addBlock,
  getLatestBlock,
  mineAndAddBlock,
  
  isValidChain,
  replaceChain,
  emptyBlockchain,

  encode64,
  decode64
) where

import Protolude

import Control.Monad (fail)

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T 

import Address
import qualified Hash 
import qualified Key 

type Index      = Int 
type Hash       = ByteString
type Timestamp  = Integer
type Blockchain = [Block]

data Transaction = Transaction
  { fromAddress :: Address
  , toAddress   :: Address
  , amount      :: Int
  } deriving (Eq, Show, Generic, S.Serialize)

data BlockHeader = BlockHeader
  { origin       :: Address       -- ^ Address of Block miner
  , previousHash :: Hash          -- ^ Previous block hash
  , transactions :: [Transaction] -- ^ List of Transactions 
  , nonce        :: Int64         -- ^ Nonce for Proof-of-Work
  } deriving (Eq, Show, Generic, S.Serialize)

-- XXX Which fields should be in header?
data Block = Block 
  { index        :: Index         -- ^ Block height
  , header       :: BlockHeader   -- ^ Block header    
  , timestamp    :: Timestamp     -- ^ Creation timestamp
  , signature    :: ByteString    -- ^ Block hash
  } deriving (Eq, Show, Generic, S.Serialize)

genesisBlock :: Block
genesisBlock = Block 
  { index     = 0
  , header    = genesisBlockHeader
  , timestamp = 0
  , signature = ""
  }
  where 
    genesisBlockHeader = BlockHeader
      { origin       = ""
      , previousHash = "0"
      , transactions = []
      , nonce        = 0
      }

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

-- |
hashBlockHeader :: BlockHeader -> Hash 
hashBlockHeader BlockHeader{..} = Hash.getHash $ Hash.sha256 $ BS.concat
  [ rawAddress origin, previousHash, S.encode transactions, B8.pack (show nonce) ]

-- | Generate a block hash 
hashBlock :: Block -> Hash 
hashBlock = hashBlockHeader . header

-------------------------------------------------------------------------------
-- Block/chain operations 
-------------------------------------------------------------------------------

-- | Generates (mines) a new block using the `proofOfWork` function
mineBlock 
  :: MonadIO m  
  => Block          -- ^ Previous Block in chain
  -> Key.PrivateKey -- ^ Miner's private key
  -> [Transaction]  -- ^ List of transactions 
  -> m Block
mineBlock prevBlock privKey txs = do
    timestamp' <- liftIO now
    signature' <- liftIO $ Key.signS privKey (hashBlockHeader blockHeader)   
    return Block 
      { index     = index' 
      , header    = blockHeader
      , timestamp = timestamp' 
      , signature = S.encode signature' 
      } 
  where
    initBlockHeader = BlockHeader
      { origin       = origin'
      , previousHash = prevHash
      , transactions = txs
      , nonce        = 0
      }

    index'      = index prevBlock + 1
    prevHash    = hashBlock prevBlock 
    origin'     = deriveAddress (Key.toPublic privKey)
    blockHeader = proofOfWork index' initBlockHeader

proofOfWork 
  :: Int         -- ^ Difficulty measured by block index
  -> BlockHeader -- ^ Header to hash with nonce parameter
  -> BlockHeader 
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0 }  
  where
    dbits = round $ logBase (2 :: Float) $ fromIntegral idx 
    prefix = toS $ replicate dbits '0' 

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = T.take dbits $ encode64 headerHash

isValidBlock :: Block -> Block -> Maybe Text
isValidBlock prevBlock newBlock
  | index prevBlock + 1 /= index newBlock = Just "Index is invalid"
  | hashBlock prevBlock /= 
    previousHash (header newBlock)        = Just "PreviousHash is invalid"
-- | not (validateSignature newBlock)     = Just "Block Signature is invalid"
  | otherwise                             = Nothing

isValidChain :: Blockchain -> Maybe Text
isValidChain (x0:x1:xs) = isValidBlock x1 x0 <|> isValidChain (x1:xs)
isValidChain [_] = Nothing
isValidChain [] = Just "Empty chain"

emptyBlockchain :: Blockchain
emptyBlockchain = []

addBlock :: Block -> Blockchain -> Either Text Blockchain
addBlock _ [] = Left "Cannot add block to empty chain"
addBlock b (pb:bs) = 
  case isValidBlock pb b of
    Nothing -> Right $ b : pb : bs
    Just err -> Left err

-- | Get the latest block from the chain
getLatestBlock :: Blockchain -> Maybe Block
getLatestBlock = head 

-- | Generate a new block in the chain
mineAndAddBlock 
  :: MonadIO m 
  => Blockchain
  -> Key.PrivateKey
  -> [Transaction]
  -> m (Either Text (Block, Blockchain))
mineAndAddBlock chain privKey txs = do 
  let mLatestBlock = getLatestBlock chain
  case mLatestBlock of
    Nothing -> return $ Left "mineAndAddBlock: Chain is empty" 
    Just latestBlock -> do
      newBlock <- mineBlock latestBlock privKey txs 
      let eNewChain = addBlock newBlock chain
      return $ (newBlock,) <$> eNewChain

-- | Returns Nothing if chain should be replaced
replaceChain :: Blockchain -> Blockchain -> Maybe Text 
replaceChain oldChain newChain = 
  case isValidChain newChain of
    Nothing 
      | length newChain > length oldChain -> Nothing
      | otherwise -> Just "replaceChain: invalid chain"
    Just err -> Just err 

now :: IO Integer
now = round `fmap` getPOSIXTime 

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Transaction where
  toJSON (Transaction from to amnt) = 
    object [ "fromAddress" .= encode64 (S.encode from)
           , "toAddress"   .= encode64 (S.encode to)
           , "amount"      .= toJSON amnt
           ]

instance FromJSON Transaction where
  parseJSON (Object o) = do
    fromAddr' <- fmap S.decode $ decode64 =<< (o .: "fromAddress") 
    toAddr'   <- fmap S.decode $ decode64 =<< (o .: "toAddress") 
    case (,) <$> fromAddr' <*> toAddr' of
      Left err -> typeMismatch "Transaction: address" (Object o) 
      Right (fromAddr, toAddr) -> do
        amnt <- o .: "amount" 
        pure $ Transaction fromAddr toAddr amnt
  parseJSON invalid = typeMismatch "Transaction" invalid

instance ToJSON BlockHeader where
  toJSON (BlockHeader o ph txs n) =
    object [ "origin"       .= encode64 (rawAddress o)
           , "previousHash" .= encode64 ph
           , "transactions" .= toJSON txs
           , "nonce"        .= toJSON n
           ]

instance FromJSON BlockHeader where
  parseJSON (Object o) = 
    BlockHeader <$> (o .: "origin" >>= fmap mkAddress . decode64)  
                <*> (o .: "previousHash" >>= decode64)
                <*> (o .: "transactions")
                <*> (o .: "nonce") 

instance ToJSON Block where
  toJSON (Block i bh ts s) =
    object [ "blockIdx"  .= i
           , "header"    .= toJSON bh 
           , "timestamp" .= toJSON ts
           , "signature" .= encode64 s 
           ]

instance FromJSON Block where
  parseJSON (Object o) =
    Block <$>  o .: "index"
          <*> (o .: "timestamp"    >>= pure)
          <*> (o .: "nonce"        >>= pure)
          <*> (o .: "blockHash"    >>= decode64)
  parseJSON invalid = typeMismatch "Block" invalid  

encode64 :: ByteString -> Text
encode64 = T.decodeUtf8 . BS64.encode

decode64 :: (Monad m) => Text -> m ByteString
decode64 = either (panic . toS) pure . BS64.decode . toS

