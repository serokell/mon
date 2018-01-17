{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mon.Client
   ( recordCounter
   , recordGauge
   , recordTimer
   , reportEvent
   , Endpoint(..)
   ,
   ) where

import Data.ByteString (intercalate)
import Network.Socket (close, defaultProtocol, connect, addrAddress, withSocketsDo, getAddrInfo, addrFamily, SocketType(Datagram), socket)
import Network.Socket.ByteString (send, recv)
import Universum hiding (intercalate)

data StatsdMessage = StatsdMessage
    { smName :: Name
    , smValue :: Int
    , smMetricType :: MetricType
    , smRate :: Maybe Double
    , smTags :: [(Tag,Maybe Text)]
    }

data MetricType = Counter | Gauge | Timer

type Name = Text
type Tag = Text
type Rate = Double

data Endpoint = Endpoint
    { eHost :: String
    , ePort :: String
    }

encodeStatsdMessage :: StatsdMessage -> ByteString
encodeStatsdMessage StatsdMessage {..} = intercalate "|" (catMaybes [
      Just $ encodeUtf8 smName <> ":" <> show smValue
    , Just $ encodeMetricType smMetricType
    , ("@"<>) . show <$> smRate ])
    <> encodeTags smTags

encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge = "g"
encodeMetricType Timer = "ms"

encodeTags :: [(Tag, Maybe Text)] -> ByteString
encodeTags tags = if null tags then "" else "|#" <> intercalate ","  tagsBSs
    where
    tagsBSs = fmap (\(tag,tagValue) -> encodeUtf8 $ tag <> maybe "" (":"<>) tagValue)
                   tags

recordMetric :: MetricType -> Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordMetric metricType endpoint name rate tags value = sendStatsdUDP endpoint $ StatsdMessage
    { smName = name
    , smValue = value
    , smMetricType = metricType
    , smRate = Just rate
    , smTags = fmap (bimap identity $ \v -> if null v then Nothing else Just v) tags
    }

recordCounter, recordGauge, recordTimer :: Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordCounter = recordMetric Counter
recordGauge = recordMetric Gauge
recordTimer = recordMetric Timer

reportEvent :: Endpoint -> Name -> Rate -> [(Tag,Text)] -> IO ()
reportEvent endpoint name rate tags = recordMetric Counter endpoint name rate tags 0

sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage = sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP (Endpoint eHost ePort) msg = withSocketsDo $ do
    (serveraddr:_) <- getAddrInfo Nothing (Just eHost) (Just ePort)
    s <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect s (addrAddress serveraddr) >> return s
    send s msg
    close s
