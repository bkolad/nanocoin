{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put)

import Web.Scotty

import qualified Key
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as T 
import qualified Nanocoin.Network.Message as Msg 
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC 

-- | Initializes a node on the network with it's own copy of 
-- the blockchain, and invokes a p2p server and an http server. 
initNode :: Peer.Peer -> FilePath -> IO ()
initNode peer keysPath = do
  eNodeKeys <- Key.readKeys keysPath
  case eNodeKeys of
    Left err -> die err
    Right keys -> do
      nodeState <- Node.initNodeState peer keys 
      forkIO $ P2P.p2p nodeState 
      joinNetwork (Node.nodeSender nodeState) keys 
      RPC.rpcServer nodeState

-- | To Join the network, just send a valid CreateAccount transaction
joinNetwork :: Msg.MsgSender -> Key.KeyPair -> IO ()
joinNetwork nodeSender keys = do 
  -- Create account with node keys and broadcast create account 
  nodeSender . Msg.TransactionMsg =<< T.addAccountTx keys  
  -- Query the network for the latest block
  nodeSender $ Msg.QueryBlockMsg 1
