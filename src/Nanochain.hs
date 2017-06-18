{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nanochain where

import Protolude hiding (get, put)

import Control.Concurrent (MVar)
import Control.Arrow ((&&&)) 

import Data.Aeson hiding (json)
import Data.List (unzip, nub)

import Web.Scotty

import Network.Socket (HostName, PortNumber)

import qualified Nanochain.P2P as P2P
import qualified Nanochain.Block as Block

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

-- | Starts httpServer for interaction via HTTP
httpServer :: Peer -> MVar [Peer] -> MVar Block.Blockchain -> P2P.Sender -> IO ()
httpServer (Peer hostName' httpPort' p2pPort') peersMV chainMV p2pSender = 

  scotty httpPort' $ do

    get "/blocks" $ do
      blks <- liftIO $ readMVar chainMV
      json blks

    get "/mineBlock" $ do
      blk <- liftIO $ Block.getLatestBlock chainMV
      mNewBlock <- liftIO $ Block.mineAndAppendBlock chainMV blk
      case mNewBlock of
        Nothing -> text "Error mining block"
        Just newBlock -> do 
          putStrLn $ "Adding block with hash: " <> 
            Block.encode64 (Block.blockHash newBlock)
          liftIO $ p2pSender $ P2P.RespLatestBlock newBlock 
          json newBlock 

    get "/peers" $ do 
      peers <- liftIO $ readMVar peersMV
      json peers

    -- | Should be POST but can't query with browser unless GET 
    get "/addPeer/:httpPort" $ do
      newHttpPort <- param "httpPort"
      portsInUse <- liftIO $ getPortsInUse peersMV
      let errMsg = "Port " <> show newHttpPort <> " in use."
      if newHttpPort `elem` portsInUse
         then text errMsg
      else do
        let newPeer = Peer hostName' newHttpPort p2pPort'
        void $ liftIO $ forkIO $ 
          initNode peersMV newPeer
        liftIO $ modifyMVar_ peersMV $ return . (:) newPeer
        json newPeer

-- | Initializes a node on the network with it's own copy of 
--   the blockchain, and invokes a p2p server and an http server 
--   listening on `p2pPort` and `httpPort` respectively.
initNode :: MVar [Peer] -> Peer -> IO ()
initNode peersMV peer@(Peer hostName' _ p2pPort') = do
  chainMV <- newMVar [Block.genesisBlock]
  nodeSender <- P2P.p2p hostName' p2pPort' chainMV
  P2P.connectToPeers nodeSender
  httpServer peer peersMV chainMV nodeSender 

-- | Initializes all default nodes 
initNodes :: MVar [Peer] -> IO ()
initNodes peersMV = do
  peers <- readMVar peersMV
  forM_ peers $ \peer -> 
    forkIO $ initNode peersMV peer 

type HttpPort = Int
type P2PPort = PortNumber
data Peer = Peer
  { hostName :: HostName
  , httpPort :: HttpPort
  , p2pPort  :: P2PPort
  } 

instance ToJSON Peer where
  toJSON (Peer hn hp pp) = 
    object [ "hostName" .= toJSON hn  
           , "httpPort" .= toJSON hp 
           , "p2pPort"  .= (fromIntegral pp :: Int)
           ]
 
defaultPeers :: IO (MVar [Peer])
defaultPeers = newMVar peers
  where 
    p2pPort' = 8000
    httpPorts = [3000,3001,3002]
    peers = Peer "224.0.0.1" <$> httpPorts <*> [p2pPort']

-- | Returns ports in use by the blockchain network
getPortsInUse :: MVar [Peer] -> IO [Int]
getPortsInUse peersMV = do
  peers <- readMVar peersMV
  let (httpPorts,p2pPorts) = unzip $ 
         map (httpPort &&& (fromIntegral . p2pPort)) peers
  return $ nub $ httpPorts ++ p2pPorts

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

main :: IO ()
main = do
  peersMV <- defaultPeers
  initNodes peersMV
  forever $ threadDelay 1000000

