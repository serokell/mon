-- | This module encapsulates the usage of the @network@ library.
module Mon.Network
       ( Endpoint
       , resolveEndpoint

       , withSocket
       , sendStatsdUdp
       ) where

import Universum

import Data.Maybe (listToMaybe)
import Network.Socket (AddrInfo (..), Family (AF_INET), ProtocolNumber, Socket,
                       SocketType (Datagram), addrAddress, addrFamily, close, connect, defaultHints,
                       defaultProtocol, getAddrInfo, socket, withSocketsDo)
import Network.Socket.ByteString (send)

import Mon.Network.Statsd (StatsdMessage, encodeStatsdMessage)


-- | Host and port of the monitoring server.
type Endpoint = (Text, Int)

-- | Resolve an endpoint.
-- Returns the first 'AddrInfo' that matches.
resolveEndpoint :: Endpoint -> IO (Maybe AddrInfo)
resolveEndpoint (host, port) =
    let hints = defaultHints { addrFamily = AF_INET }
    in listToMaybe <$> getAddrInfo (Just hints)
                                   (Just $ toString host)
                                   (Just $ show port)

-- | An exception safe function for working with sockets.
-- Parameters are the same as for 'open'.
withSocket :: Family -> SocketType -> ProtocolNumber
           -> (Socket -> IO a) -> IO a
withSocket fm st pn = bracket (socket fm st pn) close

-- | Function that sends a 'StatsdMessage' as a UDP message.
sendStatsdUdp :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUdp ep = sendUdp ep . encodeStatsdMessage

sendUdp :: Endpoint -> ByteString -> IO ()
sendUdp ep msg = withSocketsDo $ do
    msa <- resolveEndpoint ep
    whenJust msa $ \sa ->
        withSocket (addrFamily sa) Datagram defaultProtocol $ \s ->
            void $ connect s (addrAddress sa) *> send s msg
