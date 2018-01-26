-- | This module encapsulates the usage of the @network@ library.
module Mon.Network
       ( Endpoint
       , sendStatsdUDP
       ) where

import Universum

import Network.Socket (AddrInfo (..), Family (AF_INET), SocketType (Datagram), addrAddress,
                       addrFamily, close, connect, defaultHints, defaultProtocol, getAddrInfo,
                       socket, withSocketsDo)
import Network.Socket.ByteString (send)

import Mon.Network.Statsd (StatsdMessage, encodeStatsdMessage)


-- | Host and port of the monitoring server.
type Endpoint = (Text, Int)

-- | Function that sends a 'StatsdMessage' as a UDP message.
sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage =
    sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP (host, port) msg = withSocketsDo $ do
    let hints = defaultHints { addrFamily = AF_INET
                             , addrSocketType = Datagram
                             }
    serveraddr : _ <- getAddrInfo (Just hints)
                                  (Just $ toString host)
                                  (Just $ show port)
    bracket (socket (addrFamily serveraddr) Datagram defaultProtocol)
            close
            (\s -> do
                connect s (addrAddress serveraddr)
                void $ send s msg
            )
