{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNanocoin
) where

import Protolude hiding (get, put)

import Web.Scotty

import qualified Nanocoin.Network.Message as Msg 
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC 
import qualified Nanocoin.Block as Block

-- | Initializes a node on the network with it's own copy of 
-- the blockchain, and invokes a p2p server and an http server. 
initNode :: Peer.Peer -> MVar [Peer.Peer] -> IO ()
initNode peer peersMV = do
    nodeState <- Node.initNodeState peer peersMV 
    forkIO $ P2P.p2p nodeState 
    connectToPeers $ Node.nodeSender nodeState
    RPC.rpcServer nodeState
  where 
    -- | Queries latest block from peers
    connectToPeers :: Msg.MsgSender -> IO ()
    connectToPeers nodeSender = nodeSender Msg.QueryLatestBlock 

-- | Initializes all default nodes 
initNodes :: MVar [Peer.Peer] -> IO ()
initNodes peersMV = do
  peers <- readMVar peersMV
  forM_  peers $ \peer -> 
    forkIO $ initNode peer peersMV 

addPeerServer :: Int -> MVar [Peer.Peer] -> IO ()
addPeerServer port peersMV = scotty port $ 
  get "/addPeer/:rpcPort" $ do
    newRPCPort <- param "rpcPort"
    currentPeers <- liftIO $ readMVar peersMV
    let portsInUse = port : Peer.getPortsInUse currentPeers
    let errMsg = "Port " <> show newRPCPort <> " in use."
    if newRPCPort `elem` portsInUse
       then text errMsg
    else do
      let newPeer = Peer.mkPeer newRPCPort
      liftIO $ forkIO $ initNode newPeer peersMV 
      json newPeer

initNanocoin :: IO ()
initNanocoin = do
  peersMV <- newMVar Peer.bootNodes
  initNodes peersMV
  addPeerServer 8545 peersMV

