

module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get)

import Web.Scotty

import qualified Nanocoin.Block as Block
import Nanocoin.Network.Node (NodeState(..), getNodePeers, getNodeChain
                              ,setNodeChain, getPortsInUse)
import Nanocoin.Network.Peer
import qualified Nanocoin.Network.Message as Msg

import qualified Key

-------------------------------------------------------------------------------
-- RPC (HTTP) Server
-------------------------------------------------------------------------------

-- | Starts an RPC server for interaction via HTTP
rpcServer :: NodeState -> IO ()
rpcServer nodeState = do 

  let (Peer hostName p2pPort rpcPort) = nodeConfig nodeState 
  let p2pSender = nodeSender nodeState
 
  scotty rpcPort $ do

    get "/blocks" $ do
      blks <- getNodeChain nodeState
      json blks

    get "/mineBlock" $ do
      chain <- getNodeChain nodeState
      let privKey = Key.privateKey $ nodeKeys nodeState
      mRes <- Block.mineAndAddBlock chain privKey ([] {- XXX MemPool -}) 
      case mRes of
        Left err -> text $ toS err
        Right (newBlock, newChain) -> do 
          putStrLn $ "Adding block with hash: " <> 
            Block.encode64 (Block.hashBlock newBlock)
          setNodeChain nodeState newChain
          liftIO $ p2pSender $ Msg.RespLatestBlock newBlock 
          json newBlock 

    get "/peers" $ do 
      peers <- getNodePeers nodeState
      json peers

    get "/balance" $ do
      json ("TODO" :: Text)
