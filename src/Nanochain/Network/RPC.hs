

module Nanochain.Network.RPC (
  rpcServer
) where

import Protolude hiding (get)

import Web.Scotty

import qualified Nanochain.Block as Block
import Nanochain.Network.Node (NodeState(..), getNodePeers, getNodeChain
                              ,setNodeChain, getPortsInUse)
import Nanochain.Network.Peer
import qualified Nanochain.Network.Message as Msg

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
      mRes <- Block.mineAndAddBlock chain 
      case mRes of
        Left err -> text $ toS err
        Right (newBlock, newChain) -> do 
          putStrLn $ "Adding block with hash: " <> 
            Block.encode64 (Block.blockHash newBlock)
          setNodeChain nodeState newChain
          liftIO $ p2pSender $ Msg.RespLatestBlock newBlock 
          json newBlock 

    get "/peers" $ do 
      peers <- getNodePeers nodeState
      json peers

