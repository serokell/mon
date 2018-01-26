-- | This module encapsulates usage of 'network' library UDP sending.

module Mon.Network
       ( Endpoint
       , sendStatsdUDP
       ) where

import Universum

import Network.Socket (AddrInfo (..), Family (AF_INET), SocketType (Datagram), addrAddress,
                       addrFamily, close, connect, defaultHints, defaultProtocol, getAddrInfo,
                       socket, withSocketsDo)
import Network.Socket.ByteString (send)

import Mon.Types (StatsdMessage, encodeStatsdMessage)

type Endpoint = (Text, Int)


-- | Function that sends UDP message containing 'StatsdMessage'.
sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage =
    sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP (host,port) msg = withSocketsDo $ do
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
