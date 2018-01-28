-- | Functions for sending metrics and events to a monitoring server.
module Mon
       ( recordCounter
       , recordGauge
       , recordTimer
       , reportEvent
       ) where

import Universum

import Mon.Network (Endpoint, sendStatsdUdp)
import Mon.Network.Statsd (StatsdMessage (..))
import Mon.Types (MetricType (..), Name, Rate, Tag)
import System.Random (randomRIO)


-- | Record a new value for a metric.
recordMetric :: MetricType  -- ^ Type of the metric
             -> Endpoint    -- ^ Server to send data to
             -> Name        -- ^ Name of the metric
             -> Rate        -- ^ Probability of the data being actually sent
             -> [Tag]       -- ^ List of tags (labels) to attach
             -> Int         -- ^ Value of the metric
             -> IO ()
recordMetric metricType endpoint name rate tags value = do
    send <- (<= rate) <$> randomRIO (0, 1)
    when send $ sendStatsdUdp endpoint StatsdMessage
        { smName = name
        , smValue = value
        , smMetricType = metricType
        , smRate = Just rate
        , smTags = tags
        }

-- | Record a new counter, gauge or timer.
recordCounter, recordGauge, recordTimer
    :: Endpoint  -- ^ Server to send data to
    -> Name      -- ^ Name of the metric
    -> Rate      -- ^ Probability of the data being actually sent
    -> [Tag]     -- ^ List of tags (labels) to attach
    -> Int       -- ^ Value of the metric
    -> IO ()
recordCounter = recordMetric Counter
recordGauge   = recordMetric Gauge
recordTimer   = recordMetric Timer

-- | Report a new event.
-- It is guaranteed that the data will be dispatched immediately.
reportEvent :: Endpoint  -- ^ Server to send data to
            -> Name      -- ^ Name of the event
            -> Rate      -- ^ Probability of the data being actually sent
            -> [Tag]     -- ^ List of tags (labels) to attach
            -> IO ()
reportEvent endpoint name rate tags =
    recordMetric Counter endpoint name rate tags 1
