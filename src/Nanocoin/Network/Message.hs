{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Message (
  Msg(..),
  MsgSender,
  MsgReceiver,
) where

import Protolude hiding (msg)

import qualified Data.Serialize as S

import Nanocoin.Block 
import Nanocoin.Transaction 
import qualified Nanocoin.Network.Multicast as M 
import qualified Nanocoin.Network.Peer as Peer 

data Msg 
  = QueryLatestBlock
  | QueryBlockchain
  | RespBlockchain Blockchain
  | RespLatestBlock Block
  | NewTransaction Transaction
  deriving (Eq, Show, Generic, S.Serialize)

type MsgSender = M.Sender Msg
type MsgReceiver = M.Receiver Msg

