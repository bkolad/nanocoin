{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Nanochain.P2P (
  Msg(..),
  Sender,
  p2p,
  connectToPeers,
) where

import Protolude hiding (msg)

import qualified Data.Serialize as S

import Network.Socket (HostName, PortNumber)

import qualified Multicast as M
import qualified Nanochain.Block as Block

-------------------------------------------------------------------------------
-- P2P
-------------------------------------------------------------------------------

data Msg 
  = QueryLatestBlock
  | QueryBlockchain
  | RespBlockchain Block.Blockchain
  | RespLatestBlock Block.Block   
  deriving (Eq, Show, Generic, S.Serialize)

type Sender = Msg -> IO ()

-- | Initializes a p2p node and returns a sender function so that 
--   the http server can broadcast messages to the p2p network
p2p 
  :: HostName 
  -> PortNumber 
  -> MVar Block.Blockchain 
  -> IO Sender
p2p hostname p2pPort' chain = do -- TODO: Take buffer size as argument, max size of blockchain
  (nodeReceiver, nodeSender) <- M.initMulticast hostname p2pPort' 65536
  void $ forkIO $ forever $ nodeReceiver >>= -- | Forever handle messages 
    either (logThread "p2p") (msgHandler nodeSender chain . fst) 
  return nodeSender

-- | Main dispatch function to handle all messages received from network
msgHandler :: Sender -> MVar Block.Blockchain -> Msg -> IO ()
msgHandler sender chain msg' = do
  prefix <- mkThreadDebugPrefix "msgHandler" 
  logThread prefix $ "Received Msg: " <> (show msg' :: Text) 
  case msg' of
    QueryLatestBlock -> do
      mBlock <- Block.getLatestBlock chain
      case mBlock of
        Nothing -> logThread prefix "no block to return"
        Just block -> sender $ RespLatestBlock block
    QueryBlockchain -> sender . RespBlockchain =<< readMVar chain
    RespLatestBlock block -> do
      mMsg <- handleResponse chain [block] 
      forM_ mMsg sender 
    RespBlockchain blockchain -> do
      mMsg <- handleResponse chain blockchain
      forM_ mMsg sender 

handleResponse :: MVar Block.Blockchain -> Block.Blockchain -> IO (Maybe Msg)
handleResponse chain chainResponse = do
  let logThread' = logThread "handleResponse"
  case head chainResponse of
    Nothing -> do 
      logThread' "Empty response chain..." 
      return Nothing 
    Just latestBlockRec -> do
      mLatestBlockHeld <- Block.getLatestBlock chain
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
          Block.addBlockMVar latestBlockRec chain
          return $ Just $ RespLatestBlock latestBlockRec
      | length chainResponse == 1 = return $ Just QueryBlockchain
      | otherwise = do 
          Block.setChain chain chainResponse 
          return $ Just $ RespLatestBlock latestBlockRec

-- | Queries latest block from peers
connectToPeers :: Sender -> IO ()
connectToPeers nodeSender = nodeSender QueryLatestBlock 

-------------------------------------
-- DEBUG
-------------------------------------

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
