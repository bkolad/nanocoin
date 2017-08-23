{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Node (
  NodeState(..),

  initNodeState,

  getNodeAddress,

  getBlockChain,
  modifyBlockChain_,
  setBlockChain,

  applyBlock,
  getLatestBlock,

  getLedger,
  setLedger,

  getMemPool,
  modifyMemPool_,
  purgeMemPool,

) where

import Protolude

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..))
import qualified Data.Text as T

import Address (Address)

import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tx
import qualified Nanocoin.Ledger as Ledger
import qualified Nanocoin.MemPool as MemPool
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Peer as Peer

import qualified Key

data NodeState = NodeState
  { nodeConfig   :: Peer.Peer
  , nodeChain    :: MVar Block.Blockchain
  , nodeKeys     :: Key.KeyPair
  , nodeSender   :: Msg.MsgSender
  , nodeReceiver :: Msg.MsgReceiver
  , nodeLedger   :: MVar Ledger.Ledger
  , nodeMemPool  :: MVar MemPool.MemPool
  }

initNodeState
  :: Peer.Peer
  -> Block.Block
  -> Key.KeyPair
  -> IO NodeState
initNodeState self genesisBlock keys = do
  let (Peer.Peer hn p2pPort _) = self
  chainMV  <- newMVar [genesisBlock]
  (receiver, sender) <- M.initMulticast hn p2pPort 65536
  ledgerMV <- newMVar mempty
  memPoolMV <- newMVar mempty
  return NodeState
    { nodeConfig   = self
    , nodeChain    = chainMV
    , nodeKeys     = keys
    , nodeSender   = sender
    , nodeReceiver = receiver
    , nodeLedger   = ledgerMV
    , nodeMemPool  = memPoolMV
    }

-------------------------------------------------------------------------------
-- NodeState Updates
-------------------------------------------------------------------------------

modifyNodeState_
  :: MonadIO m
  => NodeState             -- ^ NodeState
  -> (NodeState -> MVar a) -- ^ NodeState field
  -> (a -> IO a)           -- ^ Modifying function
  -> m ()
modifyNodeState_ nodeState f = liftIO . modifyMVar_ (f nodeState)

modifyBlockChain_
  :: MonadIO m
  => NodeState
  -> (Block.Blockchain -> Block.Blockchain)
  -> m ()
modifyBlockChain_ nodeState f = modifyNodeState_ nodeState nodeChain (pure . f)

-- | Warning: Unsafe replace chain. Use 'setBlockChain' to safely update chain
setBlockChain :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setBlockChain nodeState chain = modifyBlockChain_ nodeState (const chain)

applyBlock
  :: MonadIO m
  => NodeState
  -> Block.Block
  -> Block.Block
  -> m ()
applyBlock nodeState prevBlock  block = do
  ledger <- getLedger nodeState
  case Block.validateAndApplyBlock ledger prevBlock block of
    Left err -> putText $ show err
    Right (ledger', itxs)
      | null itxs -> do
          putText "applyBlock: Block is valid. Applying block..."
          -- If no invalid transactions, add block to chain
          modifyBlockChain_ nodeState (block:)
          -- Remove stale, invalid transactions
          purgeMemPool nodeState
          -- Remove transactions in block from memPool
          let blockTxs = Block.transactions block
          modifyMemPool_ nodeState $ MemPool.removeTransactions blockTxs
          -- Update ledger to new ledger state
          setLedger nodeState ledger'
      | otherwise -> putText $
          (<>) "applyBlock:\n" $
            T.unlines $ map ((<>) "\t" . show) itxs

setLedger :: MonadIO m => NodeState -> Ledger.Ledger -> m ()
setLedger nodeState ledger =
  modifyNodeState_ nodeState nodeLedger $ \_ ->
    putText "setLedger: Updating Ledger..." >> pure ledger

modifyMemPool_
  :: MonadIO m
  => NodeState
  -> (MemPool.MemPool -> MemPool.MemPool)
  -> m ()
modifyMemPool_ nodeState f =
  modifyNodeState_ nodeState nodeMemPool (pure . f)

-- | Removes stale transactions, returning them
purgeMemPool
  :: MonadIO m
  => NodeState
  -> m [Tx.InvalidTx]
purgeMemPool nodeState = do
  ledger <- getLedger nodeState
  txs <- MemPool.unMemPool <$> getMemPool nodeState
  let (ledger', invalidTxErrs) = Tx.applyTransactions ledger txs
  let invalidTxs = map (\(Tx.InvalidTx tx _) -> tx) invalidTxErrs
  modifyMemPool_ nodeState $ MemPool.removeTransactions invalidTxs
  return invalidTxErrs

resetMemPool
  :: MonadIO m
  => NodeState
  -> m ()
resetMemPool nodeState = do
  putText "resetMemPool: Resetting memPool..."
  modifyMemPool_ nodeState (const mempty)

-------------------------------------------------------------------------------
-- NodeState Querying
-------------------------------------------------------------------------------

getNodeAddress :: NodeState -> Address
getNodeAddress = Address.deriveAddress . fst . nodeKeys

getBlockChain :: MonadIO m => NodeState -> m Block.Blockchain
getBlockChain = liftIO . readMVar . nodeChain

getLatestBlock :: MonadIO m => NodeState -> m (Maybe Block.Block)
getLatestBlock = fmap head . getBlockChain

getLedger :: MonadIO m => NodeState -> m Ledger.Ledger
getLedger = readMVar' . nodeLedger

getMemPool :: MonadIO m => NodeState -> m MemPool.MemPool
getMemPool = readMVar' . nodeMemPool

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f
