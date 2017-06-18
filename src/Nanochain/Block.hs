{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Nanochain.Block (
  Block(..),
  Blockchain,
  hashBlockParams, 
  calcBlockHash,
  validateBlock,
  genesisBlock,

  proofOfWork,
  generateNextBlock,
  addBlock,
  addBlockMVar,
  getLatestBlock,
  mineAndAppendBlock,
  
  isValidChain,
  setChain,
  replaceChain,
  emptyBlockchain,

  encode64,
  decode64
) where

import Protolude

import Crypto.Hash

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

type Index      = Integer
type Hash       = ByteString
type Timestamp  = Integer
type BlockData  = ByteString
type Blockchain = [Block]

data Block = Block 
  { blockIdx     :: Index        -- ^ Block height
  , previousHash :: Hash         -- ^ Previous block hash
  , timestamp    :: Timestamp    -- ^ Creation timestamp
  , blockData    :: BlockData    -- ^ Block data
  , nonce        :: Int64        -- ^ Nonce for Proof-of-Work
  , blockHash    :: Hash         -- ^ Block hash
  } deriving (Eq, Show, Generic, S.Serialize)

genesisBlock :: Block
genesisBlock = Block index previousHash' timestamp' blockData' nonce' hash' 
  where
    index         = 0
    previousHash' = "0" -- empty for genesis block
    timestamp'    = 0
    blockData'    = "<<Genesis block data>>"
    nonce'        = 0
    hash'         = hashBlockParams index previousHash' timestamp' blockData' nonce'

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

sha3_256 :: ByteString -> ByteString
sha3_256 = BA.convert . hashWith SHA3_256

hashBlockParams
  :: Index 
  -> Hash 
  -> Timestamp 
  -> BlockData 
  -> Int64
  -> ByteString
hashBlockParams i p t d n = sha3_256 $ BS.concat 
  [ B8.pack (show i), p, B8.pack (show t), d, B8.pack (show n)]

calcBlockHash :: Block -> Hash
calcBlockHash Block{..} = hashBlockParams blockIdx previousHash timestamp blockData nonce 

validateBlock :: Block -> Bool
validateBlock block = calcBlockHash block == blockHash block

-------------------------------------------------------------------------------
-- Block/chain operations 
-------------------------------------------------------------------------------

-- | Generates (mines) a new block using the `proofOfWork` function
generateNextBlock :: Block -> Timestamp -> BlockData -> Block
generateNextBlock prevBlock ts blockData =
  let index      = blockIdx prevBlock + 1
      prevHash   = blockHash prevBlock 
      nonce'     = proofOfWork index prevHash ts blockData
      newBlkHash = hashBlockParams index prevHash ts blockData nonce'
  in Block index prevHash ts blockData nonce' newBlkHash

proofOfWork 
  :: Index
  -> Hash
  -> Timestamp
  -> BlockData
  -> Int64
proofOfWork idx prevHash ts blockData' = calcNonce 0  
  where
    dbits = round $ logBase (2 :: Float) $ fromIntegral idx 
    prefix = toS $ replicate dbits '0' 

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        hash' = hashBlockParams idx prevHash ts blockData' n 
        prefix' = T.take dbits $ encode64 hash' 

isValidBlock :: Block -> Block -> Maybe Text
isValidBlock previousBlock newBlock
  | blockIdx previousBlock + 1  /= blockIdx newBlock     = Just "Index is invalid"
  | blockHash previousBlock     /= previousHash newBlock = Just "PreviousHash is invalid"
  | calcBlockHash newBlock      /= blockHash newBlock    = Just "Hash is invalid"
  | otherwise                                            = Nothing

isValidChain :: Blockchain -> Maybe Text
isValidChain (x0:x1:xs) = isValidBlock x1 x0 <|> isValidChain (x1:xs)
isValidChain [_] = Nothing
isValidChain [] = Just "Empty chain"

emptyBlockchain :: Blockchain
emptyBlockchain = []

addBlock :: Block -> Blockchain -> Blockchain
addBlock _ [] = []
addBlock b (pb:bs) 
  | isNothing (isValidBlock pb b) = b : pb : bs
  | otherwise = pb : bs

addBlockMVar :: Block -> MVar Blockchain -> IO ()
addBlockMVar b = flip modifyMVar_ (return . addBlock b) 

-- | Get the latest block from the chain
getLatestBlock :: MVar Blockchain -> IO (Maybe Block)
getLatestBlock chain = head <$> readMVar chain

-- | Generate a new block in the chain
mineAndAppendBlock :: MVar Blockchain -> Maybe Block -> IO (Maybe Block)
mineAndAppendBlock _ Nothing = pure Nothing
mineAndAppendBlock chain (Just latestBlock) = modifyMVar chain $ \chain' -> do
  ts <- now
  let newBlock = generateNextBlock latestBlock ts "" -- XXX: empty for now
  let newChain = addBlock newBlock chain'
  return (newChain, Just newBlock)

-- | Returns Nothing if chain should be replaced
replaceChain :: Blockchain -> Blockchain -> Maybe Text 
replaceChain oldChain newChain = case isValidChain newChain of
  Nothing 
    | length newChain > length oldChain -> Nothing
    | otherwise -> Just "replaceChain: invalid chain"
  Just err -> Just err 

-- | Replaces local block chain if new chain is longer
setChain :: MVar Blockchain -> Blockchain -> IO ()
setChain chain newChain = modifyMVar_ chain $ \oldChain -> 
  case replaceChain oldChain newChain of
    Nothing -> return newChain
    Just err -> putStrLn err >> return oldChain 

now :: IO Integer
now = round `fmap` getPOSIXTime 

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Block where
  toJSON (Block i ph t d n h) =
    object [ "blockIdx"     .= i
           , "previousHash" .= encode64 ph
           , "timestamp"    .= toJSON t
           , "blockData"    .= encode64 d
           , "nonce"        .= toJSON n
           , "blockHash"    .= encode64 h
           ]

instance FromJSON Block where
  parseJSON (Object o) =
    Block <$>  o .: "blockIdx"
          <*> (o .: "previousHash" >>= decode64)
          <*> (o .: "timestamp"    >>= pure)
          <*> (o .: "blockData"    >>= decode64)
          <*> (o .: "nonce"        >>= pure)
          <*> (o .: "blockHash"    >>= decode64)
  parseJSON invalid = typeMismatch "Block" invalid  

encode64 :: ByteString -> Text
encode64 = T.decodeUtf8 . BS64.encode

decode64 :: (Monad m) => Text -> m ByteString
decode64 = either (panic . toS) pure . BS64.decode . toS

