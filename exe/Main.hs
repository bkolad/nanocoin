module Main where

import Protolude

import Data.Maybe (fromMaybe)

import Key (defKeysPath)
import Nanocoin (initNode)
import Nanocoin.Network.Peer (mkPeer)

import Options.Applicative

data Config = Config
  { rpcPort  :: Int
  , keysPath :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config 3000 defKeysPath

main :: IO ()
main = do
    Config rpc keys <- execParser (info parser mempty)
    initNode (mkPeer rpc) keys
  where
    portParser :: Parser (Maybe Int)
    portParser = optional $
      option auto $ long "rpc-port"
                 <> short 'p'
                 <> metavar "RPC_PORT"

    keysParser :: Parser (Maybe FilePath)
    keysParser = optional $
      strOption $ long "keys"
               <> short 'k'
               <> metavar "KEYS_DIR"

    parser = Config
      <$> (fromMaybe (rpcPort defaultConfig) <$> portParser)
      <*> (fromMaybe (keysPath defaultConfig) <$> keysParser)
