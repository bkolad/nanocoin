module Main where

import Protolude

import Data.Maybe (fromMaybe)

import Nanocoin (initNode)

import Options.Applicative

data Config = Config
  { rpcPort      :: Int
  , keysPath     :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config 3000 Nothing

main :: IO ()
main = do
    Config rpc mKeys <- execParser (info parser mempty)
    initNode rpc mKeys
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
      <*> keysParser
