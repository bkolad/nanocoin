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
  genesisBlock,

  -- ** Block Hashing
  hashBlock,
  hashBlockHeader,

  -- ** Validation
  InvalidBlock(..),
  validateBlock,
  applyBlock,
  validateAndApplyBlock,

  -- ** Consensus
  proofOfWork,
  checkProofOfWork,
  mineBlock,
  getLatestBlock,


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
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Address
import Nanocoin.Ledger
import Nanocoin.Transaction (Transaction)

import qualified Hash
import qualified Key
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Ledger as Ledger

type Index      = Int
type Timestamp  = Integer
type Blockchain = [Block]

data BlockHeader = BlockHeader
  { origin       :: Address       -- ^ Address of Block miner
  , previousHash :: ByteString    -- ^ Previous block hash
  , transactions :: [Transaction] -- ^ List of Transactions
  , nonce        :: Int64         -- ^ Nonce for Proof-of-Work
  } deriving (Eq, Show, Generic, S.Serialize)

data Block = Block
  { index        :: Index         -- ^ Block height
  , header       :: BlockHeader   -- ^ Block header
  , signature    :: ByteString    -- ^ Block signature
  } deriving (Eq, Show, Generic, S.Serialize)

genesisBlock :: Block
genesisBlock = Block
  { index     = 0
  , header    = genesisBlockHeader
  , signature = ""
  }
  where
    genesisBlockHeader = BlockHeader
      { origin       = ""
      , previousHash = "0"
      , transactions = []
      , nonce        = 0
      }

-- | Get the latest block from the chain
getLatestBlock :: Blockchain -> Maybe Block
getLatestBlock = head

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

-- | Hash a block header, to be used as the prevHash field in Block
hashBlockHeader :: BlockHeader -> ByteString
hashBlockHeader BlockHeader{..} = Hash.getHash $ Hash.sha256 $ BS.concat
  [ rawAddress origin, previousHash, S.encode transactions, B8.pack (show nonce) ]

-- | Generate a block hash
hashBlock :: Block -> ByteString
hashBlock = hashBlockHeader . header

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data InvalidBlock
  = InvalidBlockSignature Text
  | InvalidBlockIndex
  | InvalidBlockHash
  | InvalidBlockNumTxs
  | InvalidBlockTx T.InvalidTx
  | InvalidPrevBlockHash
  | InvalidFirstBlock
  | InvalidOriginAddress Address
  | InvalidBlockTxs [T.InvalidTx]
  deriving (Show, Eq)

-- | Verify a block's ECDSA signature
verifyBlockSignature
  :: Ledger
  -> Block
  -> Either InvalidBlock ()
verifyBlockSignature l b =
  let hdr = header b in
  case Ledger.lookupAccount (origin hdr) l of
    Nothing -> Left $ InvalidOriginAddress (origin hdr)
    Just acc -> case S.decode (signature b) of
      Left err -> Left $ InvalidBlockSignature (toS err)
      Right sig -> do
        let validSig = Key.verify (fst acc) sig (S.encode hdr)
        unless validSig $
          Left $ InvalidBlockSignature "Could not verify block signature."

-- | Validate a block before accepting a block as new block in chain
validateBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock ()
validateBlock ledger prevBlock block
  | index block /= index prevBlock + 1 = Left InvalidBlockIndex
  | hashBlock prevBlock /= previousHash (header block) = Left InvalidPrevBlockHash
  | not (checkProofOfWork block) = Left InvalidBlockHash
  | null (transactions $ header block) = Left InvalidBlockNumTxs
  | index block == 1 = validateFirstBlock block
  | otherwise = do
      -- Verify signature of block
      verifyBlockSignature ledger block
      -- Validate all transactions w/ respect to world state
      first InvalidBlockTx $ do
        let txs = transactions $ header block
        T.validateTransactions ledger txs

-- | The first block may only contain CreateAccount transactions
validateFirstBlock :: Block -> Either InvalidBlock ()
validateFirstBlock b
  | all isCreateAccountTx (transactions $ header b) = Right ()
  | otherwise = Left InvalidFirstBlock
  where
    isCreateAccountTx tx =
      case T.header tx of
        T.TxAccount (T.CreateAccount _) -> True
        T.TxTransfer _ -> False

validateAndApplyBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock (Ledger, [T.InvalidTx])
validateAndApplyBlock ledger prevBlock block = do
  validateBlock ledger prevBlock block
  Right $ applyBlock ledger block

-- | Apply block transactions to world state
applyBlock
  :: Ledger
  -> Block
  -> (Ledger, [T.InvalidTx])
applyBlock ledger = T.applyTransactions ledger . transactions . header

-------------------------------------------------------------------------------
-- Consensus
-------------------------------------------------------------------------------

-- | Generates (mines) a new block using the `proofOfWork` function
mineBlock
  :: MonadIO m
  => Block          -- ^ Previous Block in chain
  -> Key.PrivateKey -- ^ Miner's private key
  -> [Transaction]  -- ^ List of transactions
  -> m Block
mineBlock prevBlock privKey txs = do
    signature' <- liftIO $ -- Sign the serialized block header
      Key.sign privKey (S.encode blockHeader)
    return Block
      { index     = index'
      , header    = blockHeader
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

    now :: IO Integer
    now = round `fmap` getPOSIXTime

proofOfWork
  :: Int         -- ^ Difficulty measured by block index
  -> BlockHeader -- ^ Header to hash with nonce parameter
  -> BlockHeader
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0 }
  where
    difficulty = calcDifficulty idx
    prefix = toS $ replicate difficulty '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = BS.take difficulty headerHash

-- | difficulty(block) = round(ln(index(block)))
calcDifficulty :: Int -> Int
calcDifficulty = round . logBase (2 :: Float) . fromIntegral

checkProofOfWork :: Block -> Bool
checkProofOfWork block =
    BS.isPrefixOf prefix $ hashBlock block
  where
    difficulty = calcDifficulty $ index block
    prefix = toS $ replicate difficulty '0'

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON BlockHeader where
  toJSON (BlockHeader o ph txs n) =
    object [ "origin"       .= Hash.encode64 (rawAddress o)
           , "previousHash" .= decodeUtf8 ph
           , "transactions" .= toJSON txs
           , "nonce"        .= toJSON n
           ]

instance ToJSON Block where
  toJSON (Block i bh s) =
    object [ "index"  .= i
           , "header"    .= toJSON bh
           , "signature" .= Hash.encode64 s
           ]
