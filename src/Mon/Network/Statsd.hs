{-# LANGUAGE RecordWildCards #-}

-- | Types used in the mon wire protocol.
module Mon.Network.Statsd
       ( StatsdMessage (..)
       , encodeStatsdMessage
       ) where

import Universum hiding (intercalate)

import Data.Text (intercalate)

import Mon.Types (MetricType (..), Name, Rate, Tag)


-- | A message compatible with the DogStatsD protocol.
data StatsdMessage = StatsdMessage
    { smName       :: !Name
    , smValue      :: !Int
    , smMetricType :: !MetricType
    , smRate       :: !(Maybe Rate)
    , smTags       :: ![Tag]
    } deriving Show

-- | Convert a 'StatsdMessage' into a sequence of bytes.
encodeStatsdMessage :: StatsdMessage -> ByteString
encodeStatsdMessage StatsdMessage {..} = encodeUtf8 . intercalate "|" $ catMaybes
    [ Just $ smName <> ":" <> show smValue
    , Just $ encodeMetricType smMetricType
    , ("@" <>) . show <$> smRate
    , ("#" <>) <$> encodeTags smTags
    ]

-- | Encode a 'MetricType'.
encodeMetricType :: MetricType -> Text
encodeMetricType Counter = "c"
encodeMetricType Gauge   = "g"
encodeMetricType Timer   = "ms"

-- | Encode a list of 'Tag's.
encodeTags :: [Tag] -> Maybe Text
encodeTags []   = Nothing
encodeTags tags = Just $ intercalate "," (map encodeTag tags)

-- | Encode a single 'Tag'.
encodeTag :: Tag -> Text
encodeTag (tag, val) = tag <> if null val then "" else ":" <> val
