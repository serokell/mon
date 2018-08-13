-- | Functions for sending metrics and events to a monitoring server.
module Mon
       ( recordCounter
       , recordGauge
       , recordTimer
       , reportEvent
       , recordMetric
       ) where

import Universum

import Mon.Network (Endpoint, sendStatsdUdp)
import Mon.Network.Statsd (StatsdMessage (..))
import Mon.Types (MetricType (..), Name, Rate, Tag)
import System.Random (randomRIO)


-- | Record a new value for a metric.
recordMetric :: (MonadIO m)
             => MetricType  -- ^ Type of the metric
             -> Endpoint    -- ^ Server to send data to
             -> Name        -- ^ Name of the metric
             -> Rate        -- ^ Probability of the data being actually sent
             -> [Tag]       -- ^ List of tags (labels) to attach
             -> Int         -- ^ Value of the metric
             -> m ()
recordMetric metricType endpoint name rate tags value = liftIO $ do
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
    :: (MonadIO m)
    => Endpoint  -- ^ Server to send data to
    -> Name      -- ^ Name of the metric
    -> Rate      -- ^ Probability of the data being actually sent
    -> [Tag]     -- ^ List of tags (labels) to attach
    -> Int       -- ^ Value of the metric
    -> m ()
recordCounter = recordMetric Counter
recordGauge   = recordMetric Gauge
recordTimer   = recordMetric Timer

-- | Report a new event.
-- It is guaranteed that the data will be dispatched immediately.
reportEvent :: (MonadIO m)
            => Endpoint  -- ^ Server to send data to
            -> Name      -- ^ Name of the event
            -> Rate      -- ^ Probability of the data being actually sent
            -> [Tag]     -- ^ List of tags (labels) to attach
            -> m ()
reportEvent endpoint name rate tags =
    recordMetric Counter endpoint name rate tags 1
