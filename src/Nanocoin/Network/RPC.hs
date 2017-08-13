
module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get, intercalate)

import Data.Aeson hiding (json)
import Data.Text (intercalate) 
import Web.Scotty

import qualified Data.Map as Map

import Address
import Nanocoin.Network.Node
import Nanocoin.Network.Peer

import qualified Address
import qualified Key
import qualified Nanocoin.Block as B
import qualified Nanocoin.MemPool as MP
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
      -- Attempt to mine block
      mPrevBlock <- getLatestBlock nodeState
      case mPrevBlock of
        Nothing -> text "Cannot mine block without a genesis block"
        Just prevBlock -> do
          let privKey = snd $ nodeKeys nodeState
          txs <- MP.unMemPool <$> getMemPool nodeState
          block <- B.mineBlock prevBlock privKey txs
          
          -- Are block transactions valid?
          ledger <- getLedger nodeState
          case B.validateAndApplyBlock ledger prevBlock block of
            Left err -> text $ show err
            Right (_, invalidTxErrs) 
              -- If block & txs valid, broadcast block
              | null invalidTxErrs -> do
                  putText $ "Generated block with hash:\n\t" 
                    <> decodeUtf8 (B.hashBlock block)
                  liftIO $ p2pSender $ Msg.BlockMsg block 
                  json block 
              -- If invalid, remove invalid txs from mempool
              | otherwise -> do
                  let invalidTxs = T.invalidTxs invalidTxErrs
                  modifyMemPool_ nodeState $ MP.removeTransactions invalidTxs
                  json $ Map.fromList $ zip ([1..] :: [Int]) invalidTxs 

    get "/transfer/:toAddr/:amount" $ do
      toAddr <- mkAddress <$> param "toAddr"
      amount <- param "amount"
      if Address.validateAddress toAddr
        then do
          let keys = nodeKeys nodeState
          tx <- liftIO $ T.transferTx keys toAddr amount
          liftIO . p2pSender $ Msg.TransactionMsg tx
          json tx
        else
          text "Invalid Address Supplied"

queryNodeState
  :: ToJSON a
  => NodeState
  -> (NodeState -> IO a)
  -> ActionM ()
queryNodeState nodeState f = json =<< liftIO (f nodeState)
