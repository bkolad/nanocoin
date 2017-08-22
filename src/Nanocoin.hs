{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put)

import Web.Scotty

import qualified Key
import qualified Nanocoin.Block as B
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC

-- | Initializes a node on the network with it's own copy of
-- the blockchain, and invokes a p2p server and an http server.
initNode :: Int -> Maybe FilePath -> IO ()
initNode rpcPort mKeysPath = do
  let peer = Peer.mkPeer rpcPort

  -- Initialize Node Keys
  keys <- case mKeysPath of
    Nothing -> Key.newKeyPair
    Just keysPath -> do
      eNodeKeys <- Key.readKeys keysPath
      case eNodeKeys of
        Left err   -> die err
        Right keys -> pure keys

  -- Initialize Genesis Block
  genesisBlock <- do
    eKeys <- Key.readKeys "keys/genesis"
    case eKeys of
      Left err   -> die err
      Right gkeys -> B.genesisBlock gkeys

  -- Initialize NodeState
  nodeState <- Node.initNodeState peer genesisBlock keys

  -- Fork P2P server
  forkIO $ P2P.p2p nodeState
  -- Join network by querying latest block
  joinNetwork $ Node.nodeSender nodeState
  -- Run RPC server
  RPC.rpcServer nodeState

-- | Query the network for the latest block
joinNetwork :: Msg.MsgSender -> IO ()
joinNetwork nodeSender = nodeSender $ Msg.QueryBlockMsg 1
