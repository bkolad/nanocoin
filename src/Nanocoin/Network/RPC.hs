
module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get, intercalate)

import Data.Aeson hiding (json)
import Data.Text (intercalate)
import Web.Scotty

import Data.List ((\\))
import qualified Data.Map as Map

import Address
import Nanocoin.Network.Node
import Nanocoin.Network.Peer

import qualified Address
import qualified Key
import qualified Nanocoin.Ledger as L
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

    get "/address" $
      json $ getNodeAddress nodeState

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

          putText "[0] Mining Block..."

          -- Validate transactions in mempool
          ledger <- getLedger nodeState
          txs <- MP.unMemPool <$> getMemPool nodeState
          let (ledger', invalidTxErrs) = T.applyTransactions ledger txs
          let invalidTxs = map (\(T.InvalidTx tx _) -> tx) invalidTxErrs

          -- Discard invalid transactions
          putText "[1] Discarding Invalid Transactions..."
          mapM_ (putText . show) invalidTxErrs
          modifyMemPool_ nodeState $ MP.removeTransactions invalidTxs
          let validTxs = txs \\ invalidTxs

          -- Attempt to mine block with the valid transactions
          putText "[2] Constructing new block..."
          let privKey = snd $ nodeKeys nodeState

          unless (null validTxs) $ do
            block <- B.mineBlock prevBlock privKey validTxs
            case B.validateAndApplyBlock ledger prevBlock block of
              Left err -> text $ show err
              Right (_, []) -> do
                putText $ "Generated block with hash:\n\t"
                  <> decodeUtf8 (B.hashBlock block)
                -- Broadcast block message to network
                liftIO $ p2pSender $ Msg.BlockMsg block
                -- Display the new block
                json block
              Right (_, invalidTxErrs') -> do
                -- This shouldn't happen
                putText ("Could not mine block, Invalid Transactions:" :: Text)
                json $ Map.fromList $ zip ([1..] :: [Int]) invalidTxErrs'

    get "/transfer/:toAddr/:amount" $ do
      toAddr' <- param "toAddr"
      amount <- param "amount"
      case mkAddress (encodeUtf8 toAddr') of
        Left err -> text $ toSL err
        Right toAddr -> do
          let keys = nodeKeys nodeState
          tx <- liftIO $ T.transferTransaction keys toAddr amount
          liftIO . p2pSender $ Msg.TransactionMsg tx
          json tx

queryNodeState
  :: ToJSON a
  => NodeState
  -> (NodeState -> IO a)
  -> ActionM ()
queryNodeState nodeState f = json =<< liftIO (f nodeState)
