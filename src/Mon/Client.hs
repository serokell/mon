{-# LANGUAGE RecordWildCards #-}

module Mon.Client
       ( recordCounter
       , recordGauge
       , recordTimer
       , reportEvent
       ) where

import Universum

import Mon.Types
import Mon.Network

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


