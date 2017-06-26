{-# LANGUAGE TupleSections #-}

module Nanocoin.Network.Multicast (
  HostName,
  PortNumber,

  Receiver,
  Sender,
  
  defMulticastHostName,
  initMulticast,
) where

import qualified Data.ByteString as BSS (ByteString, concat)
import qualified Data.ByteString.Lazy as BSL

import           Data.Map (Map)
import qualified Data.Map as Map 
import qualified Data.Serialize as S 
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)

import           Network.Socket (HostName, PortNumber, Socket, SockAddr)
import qualified Network.Socket.ByteString as NBS (recvFrom, sendManyTo)
import           Network.Transport.Internal (decodeInt32, encodeInt32)
import           Network.Multicast (multicastSender, multicastReceiver)

import           Protolude

--------------------------------------------------------------------------------
-- Top-level API                                                              --
--------------------------------------------------------------------------------

defMulticastHostName :: HostName 
defMulticastHostName  = "224.0.0.1"

type Sender a = a -> IO ()
type Receiver a = IO (Either Text (a, SockAddr)) 

-- | Given a hostname and a port number, initialize the multicast system.
--
-- Note: it is important that you never send messages larger than the maximum
-- message size; if you do, all subsequent communication will probably fail.
--
-- Returns a reader and a writer.
initMulticast 
  :: S.Serialize a
  => HostName    -- ^ Multicast IP
  -> PortNumber  -- ^ Port number
  -> Int         -- ^ Maximum message size
  -> IO (Receiver a, Sender a)
initMulticast host port bufferSize = do
    (sendSock, sendAddr) <- multicastSender host port
    readSock <- multicastReceiver host port
    st <- newIORef Map.empty
    return (recvBinary readSock st bufferSize, writer sendSock sendAddr)
  where
    writer :: S.Serialize a => Socket -> SockAddr -> a -> IO ()
    writer sock addr val = do
      let bytes = S.encodeLazy val
          len   = encodeInt32 (BSL.length bytes)
      NBS.sendManyTo sock (len : BSL.toChunks bytes) addr

--------------------------------------------------------------------------------
-- UDP multicast read, dealing with multiple senders                          --
--------------------------------------------------------------------------------

type UDPState = Map SockAddr BSL.ByteString

getBufferFor :: SockAddr -> UDPState -> BSL.ByteString
getBufferFor = Map.findWithDefault BSL.empty 

setBufferFor :: SockAddr -> BSL.ByteString -> UDPState -> UDPState
setBufferFor addr bytes st = case Map.lookup addr st of
  Nothing -> Map.insert addr bytes st
  Just _ -> Map.adjust (const bytes) addr st

bufferAppend :: SockAddr -> BSS.ByteString -> UDPState -> UDPState
bufferAppend addr bytes st = case Map.lookup addr st of
  Nothing -> Map.insert addr (BSL.fromChunks [bytes]) st
  Just _ -> Map.adjust (`BSL.append` BSL.fromChunks [bytes]) addr st 

recvBinary 
  :: S.Serialize a 
  => Socket 
  -> IORef UDPState 
  -> Int 
  -> IO (Either Text (a,SockAddr))
recvBinary sock st bufferSize = do
  (bytes, addr) <- recvWithLength sock st bufferSize
  return $ (,addr) <$> first toS (S.decodeLazy bytes) 

recvWithLength 
  :: Socket
  -> IORef UDPState
  -> Int
  -> IO (BSL.ByteString, SockAddr)
recvWithLength sock st bufferSize = do
  (len, addr) <- recvExact sock 4 st bufferSize
  let n = decodeInt32 . BSS.concat . BSL.toChunks $ len
  bytes <- recvExactFrom addr sock n st bufferSize
  return (bytes, addr)

-- Receive all bytes currently in the buffer
recvAll :: Socket -> IORef UDPState -> Int -> IO SockAddr
recvAll sock st bufferSize = do
  (bytes, addr) <- NBS.recvFrom sock bufferSize
  modifyIORef st $ bufferAppend addr bytes
  return addr

recvExact 
  :: Socket
  -> Int
  -> IORef UDPState
  -> Int
  -> IO (BSL.ByteString, SockAddr)
recvExact sock n st bufferSize = do
  addr  <- recvAll sock st bufferSize
  bytes <- recvExactFrom addr sock n st bufferSize
  return (bytes, addr)

recvExactFrom 
  :: SockAddr
  -> Socket
  -> Int
  -> IORef UDPState
  -> Int
  -> IO BSL.ByteString
recvExactFrom addr sock n st bufferSize = go
  where
    go :: IO BSL.ByteString
    go = do
      accAddr <- getBufferFor addr <$> readIORef st
      if BSL.length accAddr >= fromIntegral n
        then do
          let (bytes, accAddr') = BSL.splitAt (fromIntegral n) accAddr
          -- modifyIORef st $ bufferFor addr ^= accAddr'
          modifyIORef st $ setBufferFor addr accAddr' 
          return bytes
        else do
          _ <- recvAll sock st bufferSize
          go

