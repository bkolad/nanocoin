{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Node (
  NodeState(..),

  initNodeState,

  getBlockChain,
  modifyBlockChain_,
  setBlockChain,
  
  addBlock,
  getLatestBlock,
  
  getLedger,
  setLedger,
  
  getPeers,
  getPortsInUse,

  getMemPool,
  modifyMemPool_,

) where

import Protolude 

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..)) 

import qualified Nanocoin.Block as Block
import qualified Nanocoin.Ledger as Ledger 
import qualified Nanocoin.MemPool as MemPool
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Peer as Peer 

import qualified Key

data NodeState = NodeState
  { nodeConfig   :: Peer.Peer
  , nodeChain    :: MVar Block.Blockchain
  , nodePeers    :: MVar [Peer.Peer]
  , nodeKeys     :: Key.KeyPair
  , nodeSender   :: Msg.MsgSender 
  , nodeReceiver :: Msg.MsgReceiver
  , nodeLedger   :: MVar Ledger.Ledger
  , nodeMemPool  :: MVar MemPool.MemPool
  } 

initNodeState :: Peer.Peer -> MVar [Peer.Peer] -> IO NodeState
initNodeState self peersMV = do
  let (Peer.Peer hn p2pPort _) = self
  chainMV  <- newMVar [Block.genesisBlock]
  keyPair  <- Key.newKeyPair 
  (receiver, sender) <- M.initMulticast hn p2pPort 65536
  ledgerMV <- newMVar mempty
  memPoolMV <- newMVar mempty
  return NodeState 
    { nodeConfig   = self
    , nodeChain    = chainMV
    , nodePeers    = peersMV
    , nodeKeys     = keyPair 
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
  -> (Block.Blockchain -> IO Block.Blockchain) 
  -> m ()
modifyBlockChain_ nodeState = modifyNodeState_ nodeState nodeChain 

-- | Warning: Unsafe replace chain. Use 'setBlockChain' to safely update chain
setBlockChain' :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setBlockChain' nodeState chain = modifyBlockChain_ nodeState (pure . const chain) 

-- | Safe version of `setBlockChain'`, so you don't replace with a shorter chain
setBlockChain :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setBlockChain nodeState newChain = 
  modifyBlockChain_ nodeState $ \oldChain -> 
    case Block.replaceChain oldChain newChain of
      Nothing -> return newChain
      Just err -> do
        putStrLn err 
        return oldChain 

addBlock :: MonadIO m => NodeState -> Block.Block -> m ()
addBlock nodeState newBlock = 
  modifyBlockChain_ nodeState $ \chain -> 
    case Block.addBlock newBlock chain of
      Left err -> putStrLn err >> return chain
      Right newChain -> return newChain

setLedger :: MonadIO m => NodeState -> Ledger.Ledger -> m ()
setLedger nodeState ledger = 
  modifyNodeState_ nodeState nodeLedger $ \_ -> 
    putText "Updating Ledger..." >> pure ledger

modifyMemPool_
  :: MonadIO m 
  => NodeState
  -> (MemPool.MemPool -> MemPool.MemPool)
  -> m ()
modifyMemPool_ nodeState f = 
  modifyNodeState_ nodeState nodeMemPool (pure . f)

resetMemPool 
  :: MonadIO m
  => NodeState
  -> m ()
resetMemPool = flip modifyMemPool_ (const mempty) 

------------------------------------------------------------------------------- 
-- NodeState Querying
-------------------------------------------------------------------------------

getBlockChain :: MonadIO m => NodeState -> m Block.Blockchain
getBlockChain = liftIO . readMVar . nodeChain

getLatestBlock :: MonadIO m => NodeState -> m (Maybe Block.Block)
getLatestBlock = fmap head . getBlockChain

getPeers :: MonadIO m => NodeState -> m [Peer.Peer]
getPeers = liftIO . readMVar . nodePeers

getLedger :: MonadIO m => NodeState -> m Ledger.Ledger
getLedger = readMVar' . nodeLedger

-- | Returns ports in use by the blockchain network
getPortsInUse :: MonadIO m => NodeState -> m [Int]
getPortsInUse nodeState = do
    peers <- getPeers nodeState
    return $ Peer.getPortsInUse peers 

getMemPool :: MonadIO m => NodeState -> m MemPool.MemPool
getMemPool = readMVar' . nodeMemPool

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar 

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f

