{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanochain.Network.Peer (
  Peer(..),
  getPeerPorts,
  bootNodes,
  mkPeer,
  getPortsInUse,
) where

import Protolude

import Control.Arrow ((&&&))
import Data.Aeson (ToJSON(..))
import Data.List (nub, unzip)
import qualified Data.Serialize as S

import Nanochain.Network.Multicast

type RPCPort = Int

type P2PPort = PortNumber

instance ToJSON PortNumber where
  toJSON = toJSON . (show :: PortNumber -> Text) 

instance S.Serialize PortNumber where
  put = S.putWord16be . fromIntegral 
  get = fromIntegral <$> S.getWord16be 

data Peer = Peer
  { hostName :: HostName
  , p2pPort  :: P2PPort
  , rpcPort  :: RPCPort 
  } deriving (Eq, Show, Generic, ToJSON, S.Serialize)

defP2PPort :: P2PPort
defP2PPort = 8001

bootNodes :: [Peer]
bootNodes = map mkPeer initPorts 
  where 
    initPorts = [3000,3001,3002]

mkPeer :: RPCPort -> Peer
mkPeer = Peer defMulticastHostName defP2PPort 

getPeerPorts :: Peer -> (RPCPort, P2PPort)
getPeerPorts = rpcPort &&& p2pPort

getPortsInUse :: [Peer] -> [Int]
getPortsInUse peers = 
    nub $ httpPorts ++ map fromIntegral p2pPorts
  where
    (httpPorts, p2pPorts) = getPorts peers
    getPorts = unzip . map getPeerPorts 
