-- | This module encapsulates usage of 'network' library UDP sending.

module Mon.Network
       ( Endpoint
       , sendStatsdUDP
       ) where

import Universum

import Network.Socket (close, defaultProtocol, connect, addrAddress
                      , withSocketsDo, getAddrInfo, addrFamily
                      , SocketType(Datagram), socket)
import Network.Socket.ByteString (send)

import Mon.Types (StatsdMessage, encodeStatsdMessage)

type Endpoint = (Text, Int)


-- | Function that sends UDP message containing 'StatsdMessage'.
sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage =
    sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP (host,port) msg = withSocketsDo $ do
    (serveraddr:_) <- getAddrInfo Nothing
                                  (Just $ toString host) (Just $ show port)
    bracket (socket (addrFamily serveraddr) Datagram defaultProtocol)
             close
            (\s -> do
                connect s (addrAddress serveraddr)
                void $ send s msg
            )
