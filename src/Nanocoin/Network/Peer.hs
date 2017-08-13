{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Peer (
  Peer(..),
  mkPeer,
) where

import Protolude

import Control.Arrow ((&&&))
import Data.Aeson (ToJSON(..))
import Data.List (nub, unzip)
import qualified Data.Serialize as S

import Nanocoin.Network.Multicast

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

mkPeer :: RPCPort -> Peer
mkPeer = Peer defMulticastHostName defP2PPort 
