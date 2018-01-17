{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mon.Client
   (
   recordTimer
   ) where

import Data.ByteString (intercalate)
import Data.Time (UTCTime)
import Network.Socket (close, defaultProtocol, connect, addrAddress, withSocketsDo, getAddrInfo, addrFamily, SocketType(Datagram), socket)
import Network.Socket.ByteString (send, recv)
import Universum hiding (intercalate)

data StatsdMessage = StatsdMessage
    { smDataType :: DataType
    , smTags :: [(Tag,Maybe Text)]
    }

data DataType = MetricDT Metric
              | EventDT Event

data Metric = Metric
    { mName :: Text
    , mValue :: Int
    , mMetricType :: MetricType
    , mRate :: Maybe Double
    }

data MetricType = Counter | Gauge | Timer

data Event = Event
    { eTitle :: Text
    , eText :: Text
    , eTimestamp :: Maybe UTCTime
    , eHostname :: Maybe Text
    , eAggregationKey :: Maybe Text
    , ePriority :: Maybe Text
    , eSourceTypeName :: Maybe Text
    , eAlertType :: Maybe Text
    }

type Name = Text
type Title = Text
type Tag = Text
type Rate = Double

data Endpoint = Endpoint
    { eHost :: String
    , ePort :: String
    }

encodeStatsdMessage :: StatsdMessage -> ByteString
encodeStatsdMessage (StatsdMessage smDataType smTags) =
       encodeDataType smDataType
    <> encodeTags smTags

encodeDataType :: DataType -> ByteString
encodeDataType (MetricDT metric) = encodeMetric metric
encodeDataType (EventDT event) = encodeEvent event

encodeMetric :: Metric -> ByteString
encodeMetric Metric {..} = intercalate "|" $ catMaybes [
      Just $ encodeUtf8 mName <> ":" <> show mValue
    , Just $ encodeMetricType mMetricType
    , ("@"<>) . show <$> mRate ]

encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge = "g"
encodeMetricType Timer = "ms"

encodeEvent :: Event -> ByteString
encodeEvent Event {..} = intercalate "|" $ catMaybes [
      Just $ "_e{" <> show (length title) <> "," <> show (length text) <> "}:" <> title
    , Just $ text
    , ("d:"<>) . show <$> eTimestamp
    , ("h:"<>) . encodeUtf8 <$> eHostname
    , ("k:"<>) . encodeUtf8 <$> eAggregationKey
    , ("p:"<>) . encodeUtf8 <$> ePriority
    , ("s:"<>) . encodeUtf8 <$> eSourceTypeName
    , ("t:"<>) . encodeUtf8 <$> eAlertType ]

    where
    title = encodeUtf8 eTitle
    text = encodeUtf8 eText

encodeTags :: [(Tag, Maybe Text)] -> ByteString
encodeTags tags = if null tags then "" else "|#" <> intercalate ","  tagsBSs
    where
    tagsBSs = fmap (\(tag,tagValue) -> encodeUtf8 $ tag <> maybe "" (":"<>) tagValue)
                   tags

recordDataType :: Endpoint -> DataType -> [(Tag,Text)] -> IO ()
recordDataType endpoint dataType tags = sendStatsdUDP endpoint $ StatsdMessage
    { smDataType = dataType
    , smTags = fmap (bimap identity $ \v -> if null v then Nothing else Just v) tags
    }

recordMetric :: MetricType -> Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordMetric metricType endpoint name rate tags value = recordDataType endpoint (MetricDT $ Metric
    { mName = name
    , mValue = value
    , mMetricType = metricType
    , mRate = Just rate
    })
    tags

recordCounter, recordGauge, recordTimer :: Endpoint -> Name -> Rate -> [(Tag,Text)] -> Int -> IO ()
recordCounter = recordMetric Counter
recordGauge = recordMetric Gauge
recordTimer = recordMetric Timer

reportEvent :: Endpoint -> Title -> Text -> [(Tag,Text)] -> IO ()
reportEvent endpoint title text tags = recordDataType endpoint (EventDT $ Event
    { eTitle = title
    , eText = text
    , eTimestamp = Nothing
    , eHostname = Nothing
    , eAggregationKey = Nothing
    , ePriority = Nothing
    , eSourceTypeName = Nothing
    , eAlertType = Nothing
    })
    tags

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
