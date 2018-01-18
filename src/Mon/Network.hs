{-# LANGUAGE RecordWildCards #-}

module Mon.Network
       ( Endpoint (..)
       , sendStatsdUDP
       ) where

import Network.Socket (close, defaultProtocol, connect, addrAddress, withSocketsDo, getAddrInfo, addrFamily, SocketType(Datagram), socket)
import Network.Socket.ByteString (send)
import Universum

import Mon.Types

data Endpoint = Endpoint
    { eHost :: !Text
    , ePort :: !Text
    }

sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage = sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP Endpoint {..} msg = withSocketsDo $ do
    (serveraddr:_) <- getAddrInfo Nothing (Just $ toString eHost) (Just $ toString ePort)
    s <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect s (addrAddress serveraddr)
    _ <- send s msg
    close s
