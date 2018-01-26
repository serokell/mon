-- | Monitoring Client contains functions for sending
--   metrics or events to the monitoring server

module Mon.Client
       ( recordCounter
       , recordGauge
       , recordTimer
       , reportEvent
       ) where

import Universum

import Mon.Network (Endpoint, sendStatsdUDP)
import Mon.Types (MetricType (..), Name, Rate, StatsdMessage (..), Tag)

recordMetric :: MetricType -> Endpoint -> Name -> Rate -> [Tag] -> Int -> IO ()
recordMetric metricType endpoint name rate tags value =
    sendStatsdUDP endpoint $ StatsdMessage
    { smName = name
    , smValue = value
    , smMetricType = metricType
    , smRate = Just rate
    , smTags = tags
    }

recordCounter, recordGauge, recordTimer
    :: Endpoint
    -> Name
    -> Rate
    -> [Tag]
    -> Int
    -> IO ()
recordCounter = recordMetric Counter
recordGauge   = recordMetric Gauge
recordTimer   = recordMetric Timer

reportEvent :: Endpoint -> Name -> Rate -> [Tag] -> IO ()
reportEvent endpoint name rate tags =
    recordMetric Counter endpoint name rate tags 1
