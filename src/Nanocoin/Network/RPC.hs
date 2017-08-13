

module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get)

import Data.Aeson hiding (json)
import Web.Scotty

import Address
import Nanocoin.Network.Node
import Nanocoin.Network.Peer

import qualified Address
import qualified Key
import qualified Nanocoin.Block as Block
import qualified Nanocoin.MemPool as MemPool
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg


-------------------------------------------------------------------------------
-- RPC (HTTP) Server
-------------------------------------------------------------------------------

-- | Starts an RPC server for interaction via HTTP
rpcServer :: NodeState -> IO ()
rpcServer nodeState = do

  let (Peer hostName p2pPort rpcPort) = nodeConfig nodeState
  let p2pSender = nodeSender nodeState

  scotty rpcPort $ do

    defaultHandler $ putText . toS 

    --------------------------------------------------
    -- Queries
    --------------------------------------------------

    get "/blocks" $
      queryNodeState nodeState getBlockChain

    get "/mempool" $
      queryNodeState nodeState getMemPool

    get "/ledger" $
      queryNodeState nodeState getLedger

    --------------------------------------------------
    -- Commands
    --------------------------------------------------

    get "/mineBlock" $ do
      chain <- getBlockChain nodeState
      let privKey = snd $ nodeKeys nodeState
      txs <- MemPool.unMemPool <$> getMemPool nodeState
      mRes <- Block.mineAndAddBlock chain privKey txs
      case mRes of
        Left err -> text $ toS err
        Right (newBlock, newChain) -> do
          putStrLn $ "Adding block with hash: " <>
            Block.encode64 (Block.hashBlock newBlock)
          setBlockChain nodeState newChain
          liftIO $ p2pSender $ Msg.RespLatestBlock newBlock
          json newBlock

    get "/transfer/:toAddr/:amount" $ do
      toAddr <- mkAddress <$> param "toAddr"
      amount <- param "amount"
      if Address.validateAddress toAddr
        then do
          let keys = nodeKeys nodeState
          tx <- liftIO $ T.transferTx keys toAddr amount
          liftIO . p2pSender $ Msg.NewTransaction tx
          json tx
        else
          text "Invalid Address Supplied"

queryNodeState
  :: ToJSON a
  => NodeState
  -> (NodeState -> IO a)
  -> ActionM ()
queryNodeState nodeState f = json =<< liftIO (f nodeState)
