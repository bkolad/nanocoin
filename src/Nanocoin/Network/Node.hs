{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Node (
  NodeState(..),

  initNodeState,

  getNodeChain,
  modifyNodeChain_,
  setNodeChain,
  
  addBlockNodeChain,
  getLatestBlock,
  
  getLedger,
  setLedger,
  
  getNodePeers,
  getPortsInUse,
) where

import Protolude 

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..)) 

import qualified Nanocoin.Block as Block
import qualified Nanocoin.Ledger as Ledger 
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
  } 

initNodeState :: Peer.Peer -> MVar [Peer.Peer] -> IO NodeState
initNodeState self peersMV = do
  let (Peer.Peer hn p2pPort _) = self
  chainMV  <- newMVar [Block.genesisBlock]
  keyPair  <- Key.newKeyPair 
  (receiver, sender) <- M.initMulticast hn p2pPort 65536
  ledgerMV <- newMVar mempty
  return NodeState 
    { nodeConfig   = self
    , nodeChain    = chainMV
    , nodePeers    = peersMV
    , nodeKeys     = keyPair 
    , nodeSender   = sender
    , nodeReceiver = receiver
    , nodeLedger   = ledgerMV 
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

modifyNodeChain_ 
  :: MonadIO m 
  => NodeState 
  -> (Block.Blockchain -> IO Block.Blockchain) 
  -> m ()
modifyNodeChain_ nodeState = modifyNodeState_ nodeState nodeChain 

-- | Warning: Unsafe replace chain. Use 'setNodeChain' to safely update chain
setNodeChain' :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setNodeChain' nodeState chain = modifyNodeChain_ nodeState (pure . const chain) 

-- | Safe version of `setNodeChain'`, so you don't replace with a shorter chain
setNodeChain :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setNodeChain nodeState newChain = 
  modifyNodeChain_ nodeState $ \oldChain -> 
    case Block.replaceChain oldChain newChain of
      Nothing -> return newChain
      Just err -> do
        putStrLn err 
        return oldChain 

addBlockNodeChain :: MonadIO m => NodeState -> Block.Block -> m ()
addBlockNodeChain nodeState newBlock = 
  modifyNodeChain_ nodeState $ \chain -> 
    case Block.addBlock newBlock chain of
      Left err -> putStrLn err >> return chain
      Right newChain -> return newChain

getLedger :: MonadIO m => NodeState -> m Ledger.Ledger
getLedger = readMVar' . nodeLedger

setLedger :: MonadIO m => NodeState -> Ledger.Ledger -> m ()
setLedger nodeState ledger = 
  modifyNodeState_ nodeState nodeLedger $ \_ -> 
    putText "Updating Ledger..." >> pure ledger

------------------------------------------------------------------------------- 
-- NodeState Querying
-------------------------------------------------------------------------------

getNodeChain :: MonadIO m => NodeState -> m Block.Blockchain
getNodeChain = liftIO . readMVar . nodeChain

getLatestBlock :: MonadIO m => NodeState -> m (Maybe Block.Block)
getLatestBlock = fmap head . getNodeChain

getNodePeers :: MonadIO m => NodeState -> m [Peer.Peer]
getNodePeers = liftIO . readMVar . nodePeers

-- | Returns ports in use by the blockchain network
getPortsInUse :: MonadIO m => NodeState -> m [Int]
getPortsInUse nodeState = do
    peers <- getNodePeers nodeState
    return $ Peer.getPortsInUse peers 

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar 

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f

