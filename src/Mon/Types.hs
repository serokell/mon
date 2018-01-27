-- | Mon basic types.
module Mon.Types
       ( MetricType (..)
       , Name
       , Tag
       , Rate
       ) where

import Universum


-- | Enumeration of types of metrics.
data MetricType = Counter | Gauge | Timer
    deriving Show

-- | Name of a metric.
type Name = Text

-- | Tag (label).
type Tag = (Text,Text)

-- | Sample rate of a metric.
type Rate = Double
