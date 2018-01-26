{-# LANGUAGE RecordWildCards #-}

-- | Types used in the mon wire protocol.
module Mon.Network.Statsd
       ( StatsdMessage (..)
       , encodeStatsdMessage
       ) where

import Universum hiding (intercalate)

import Data.ByteString (intercalate)

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
encodeStatsdMessage StatsdMessage {..} = intercalate "|" $ catMaybes
    [ Just $ encodeUtf8 smName <> ":" <> show smValue
    , Just $ encodeMetricType smMetricType
    , ("@" <>) . show <$> smRate
    , ("#" <>) <$> encodeTags smTags
    ]

-- | Encode a 'MetricType'.
encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge   = "g"
encodeMetricType Timer   = "ms"

-- | Encode a list of 'Tag's.
encodeTags :: [Tag] -> Maybe ByteString
encodeTags []   = Nothing
encodeTags tags = Just $ intercalate "," (map encodeTag tags)
  where
    encodeTag (tag, val) = encodeUtf8 $ tag <> if null val then "" else ":" <> val
