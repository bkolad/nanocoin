{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.P2P (
  p2p,
) where

import Protolude hiding (msg)

import Control.Arrow ((&&&))

import Network.Socket (HostName, PortNumber)

import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Message as Msg 
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.Peer as Peer 
import qualified Nanocoin.Network.RPC as RPC 
import qualified Nanocoin.Block as Block

-------------------------------------------------------------------------------
-- P2P
-------------------------------------------------------------------------------

-- | Initializes a p2p node and returns a sender function so that 
-- the rpc server can broadcast messages to the p2p network
p2p :: Node.NodeState -> IO () 
p2p nodeState = do -- TODO: Take buffer size as argument, max size of blockchain
  let (sender,receiver) = Node.nodeSender &&& Node.nodeReceiver $ nodeState
  void $ forkIO $ forever $ receiver >>= -- | Forever handle messages 
    either (logThread "p2p") (handleMsg nodeState . fst) 

----------------------------------------------------------------
-- Msg Handling 
----------------------------------------------------------------

-- | Main dispatch function to handle all messages received from network
handleMsg :: Node.NodeState -> Msg.Msg -> IO ()
handleMsg nodeState msg = do
  prefix <- mkThreadDebugPrefix "handleMsg" 
  logThread prefix $ "Received Msg: " <> (show msg :: Text) 
  let nodeSender = Node.nodeSender nodeState 
  case msg of
    Msg.QueryLatestBlock -> do
      mBlock <- Node.getLatestBlock nodeState
      case mBlock of
        Nothing -> logThread prefix "no block to return"
        Just block -> nodeSender $ Msg.RespLatestBlock block
    Msg.QueryBlockchain -> do
      chain <- Node.getNodeChain nodeState 
      nodeSender $ Msg.RespBlockchain chain 
    Msg.RespLatestBlock block -> do
      mMsg <- handleResponse nodeState [block] 
      forM_ mMsg nodeSender
    Msg.RespBlockchain blockchain -> do
      mMsg <- handleResponse nodeState blockchain
      forM_ mMsg nodeSender 

handleResponse :: Node.NodeState -> Block.Blockchain -> IO (Maybe Msg.Msg)
handleResponse nodeState newChain = do
  let logThread' = logThread "handleResponse"
  localChain <- Node.getNodeChain nodeState
  case head newChain of
    Nothing -> do 
      logThread' "Empty response chain..." 
      return Nothing 
    Just latestBlockRec -> do
      mLatestBlockHeld <- Node.getLatestBlock nodeState 
      case mLatestBlockHeld of
        Nothing -> do
          logThread' "Empty local chain..." 
          return Nothing
        Just latestBlockHeld
          | Block.blockIdx latestBlockRec > 
            Block.blockIdx latestBlockHeld -> do 
              logThread' "Local chain potentially behind..."
              respond latestBlockRec latestBlockHeld
          | otherwise -> do 
              logThread' "received chain is not longer than local."
              return Nothing
  where
    respond latestBlockRec latestBlockHeld 
      | Block.blockHash latestBlockHeld == 
        Block.previousHash latestBlockRec = do
          Node.addBlockNodeChain nodeState latestBlockRec 
          return $ Just $ Msg.RespLatestBlock latestBlockRec
      | length newChain == 1 = return $ Just Msg.QueryBlockchain
      | otherwise = do 
          Node.setNodeChain nodeState newChain 
          return $ Just $ Msg.RespLatestBlock latestBlockRec

----------------------------------------------------------------
-- DEBUG
----------------------------------------------------------------

mkThreadDebugPrefix :: Text -> IO Text
mkThreadDebugPrefix funcName = do
  threadIdStr <- show <$> myThreadId 
  return $ threadIdStr <> " - " <> funcName <> ": "

-- | Set `debug` to `True` to enable logging
logThread :: Text -> Text -> IO () 
logThread funcName msg 
  | debug = do
      logMsg <- (<> msg) <$> mkThreadDebugPrefix funcName 
      print logMsg 
  | otherwise = return ()
  where -- | 
    debug = False
