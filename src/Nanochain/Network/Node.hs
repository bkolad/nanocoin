{-# LANGUAGE DeriveAnyClass #-}

module Nanochain.Network.Node (
  NodeState(..),

  initNodeState,

  getNodeChain,
  modifyNodeChain,
  setNodeChain,
  addBlockNodeChain,
  getLatestBlock,
  
  getNodePeers,
  getPortsInUse,
) where

import Protolude 

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..)) 

import qualified Nanochain.Block as Block
import qualified Nanochain.Network.Message as Msg
import qualified Nanochain.Network.Multicast as M
import qualified Nanochain.Network.Peer as Peer 

data NodeState = NodeState
  { nodeConfig   :: Peer.Peer
  , nodeChain    :: MVar Block.Blockchain
  , nodePeers    :: MVar [Peer.Peer]
  , nodeSender   :: Msg.MsgSender 
  , nodeReceiver :: Msg.MsgReceiver
  } 

initNodeState :: Peer.Peer -> MVar [Peer.Peer] -> IO NodeState
initNodeState self peersMV = do
  let (Peer.Peer hn p2pPort _) = self
  chainMV <- newMVar [Block.genesisBlock]
  (receiver, sender) <- M.initMulticast hn p2pPort 65536
  return NodeState 
    { nodeConfig   = self
    , nodeChain    = chainMV
    , nodePeers    = peersMV
    , nodeSender   = sender
    , nodeReceiver = receiver
    }

getNodeChain :: MonadIO m => NodeState -> m Block.Blockchain
getNodeChain = liftIO . readMVar . nodeChain

modifyNodeChain 
  :: MonadIO m 
  => NodeState 
  -> (Block.Blockchain -> IO Block.Blockchain) 
  -> m ()
modifyNodeChain nodeState = liftIO . modifyMVar_ (nodeChain nodeState)

setNodeChain' :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setNodeChain' nodeState chain = modifyNodeChain nodeState (pure . const chain) 

-- | Safe version of `setNodeChain'`, so you don't replace with a shorter chain
setNodeChain :: MonadIO m => NodeState -> Block.Blockchain -> m ()
setNodeChain nodeState newChain = 
  modifyNodeChain nodeState $ \oldChain -> 
    case Block.replaceChain oldChain newChain of
      Nothing -> return newChain
      Just err -> putStrLn err >> return oldChain 

addBlockNodeChain :: MonadIO m => NodeState -> Block.Block -> m ()
addBlockNodeChain nodeState newBlock = 
  modifyNodeChain nodeState $ \chain -> 
    case Block.addBlock newBlock chain of
      Left err -> putStrLn err >> return chain
      Right newChain -> return newChain

getLatestBlock :: MonadIO m => NodeState -> m (Maybe Block.Block)
getLatestBlock = fmap head . getNodeChain

getNodePeers :: MonadIO m => NodeState -> m [Peer.Peer]
getNodePeers = liftIO . readMVar . nodePeers

-- | Returns ports in use by the blockchain network
getPortsInUse :: MonadIO m => NodeState -> m [Int]
getPortsInUse nodeState = do
    peers <- getNodePeers nodeState
    return $ Peer.getPortsInUse peers 
