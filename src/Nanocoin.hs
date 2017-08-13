{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put)

import Web.Scotty

import Key

import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as T 
import qualified Nanocoin.Network.Message as Msg 
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC 

-- | Initializes a node on the network with it's own copy of 
-- the blockchain, and invokes a p2p server and an http server. 
initNode :: Peer.Peer -> IO ()
initNode peer = do
  nodeKeys <- newKeyPair
  nodeState <- Node.initNodeState peer nodeKeys 
  forkIO $ P2P.p2p nodeState 
  joinNetwork (Node.nodeSender nodeState) nodeKeys
  RPC.rpcServer nodeState

-- | To Join the network, just send a valid CreateAccount transaction
joinNetwork :: Msg.MsgSender -> KeyPair -> IO ()
joinNetwork nodeSender keys = do 
  -- Create account with node keys and broadcast create account 
  nodeSender . Msg.TransactionMsg =<< T.addAccountTx keys  
  -- Query the network for the latest block
  nodeSender $ Msg.QueryBlockMsg 1
