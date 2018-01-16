{-# LANGUAGE OverloadedStrings #-}

module Mon.Client
   (
   recordTimer
   ) where

import Data.ByteString (intercalate)
import Network.Socket (close, defaultProtocol, connect, addrAddress, withSocketsDo, getAddrInfo, addrFamily, SocketType(Datagram), socket)
import Network.Socket.ByteString (send, recv)
import Universum hiding (intercalate)

data StatsdMessage = MetricMessage Metric
                   | EventMessage   -- not finished

data Metric = Metric
    { mName :: Text
    , mValue :: Int
    , mMetricType :: MetricType
    , mRate :: Double
    , mTags :: [(Tag,Text)]
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
encodeStatsdMessage EventMessage = ""
encodeStatsdMessage (MetricMessage metric) = encodeMetric metric

encodeMetric :: Metric -> ByteString
encodeMetric (Metric mName mValue mMetricType mRate mTags) = intercalate "|" [
      encodeUtf8 mName <> ":" <> show mValue
    , encodeMetricType mMetricType
    , "@" <> show mRate ]
    <> if null mTags then "" else "|#" <> (intercalate "," $ (\(tag,tagValue) -> encodeUtf8 $ tag <> ":" <> tagValue) <$> mTags)

encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge = "g"
encodeMetricType Timer = "ms"

recordMetric :: MetricType -> Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordMetric metricType endpoint name rate tags value = sendStatsdUDP endpoint $ MetricMessage $ Metric
    { mName = name
    , mValue = value
    , mMetricType = metricType
    , mRate = rate
    , mTags = tags
    }

recordCounter, recordGauge, recordTimer :: Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordCounter = recordMetric Counter
recordGauge = recordMetric Gauge
recordTimer = recordMetric Timer

sendStatsdUDP :: Endpoint -> StatsdMessage -> IO ()
sendStatsdUDP endpoint statsdMessage = sendUDP endpoint (encodeStatsdMessage statsdMessage)

sendUDP :: Endpoint -> ByteString -> IO ()
sendUDP (Endpoint eHost ePort) msg = withSocketsDo $ bracket getSocket close talk
    where
    getSocket = do
        (serveraddr:_) <- getAddrInfo Nothing (Just eHost) (Just ePort)
        s <- socket (addrFamily serveraddr) Datagram defaultProtocol
        connect s (addrAddress serveraddr) >> return s
    talk s = do
        send s msg
        recv s 1024 >>= \resp -> putStrLn $ "Received " <> resp
