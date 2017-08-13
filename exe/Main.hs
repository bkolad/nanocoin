module Main where

import Nanocoin (initNode)
import Nanocoin.Network.Peer (mkPeer)

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> die "Please supply an RPC port number."
    [port] -> initNode (mkPeer $ read port)
    _      -> die "Please supply a single RPC port number."
