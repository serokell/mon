{-# LANGUAGE RecordWildCards #-}

-- | StatsdMessage is a representation of a statsd udp message.
module Mon.Types
       ( StatsdMessage (..)
       , MetricType (..)
       , Name
       , Tag
       , Rate
       , encodeStatsdMessage
       ) where

import Universum hiding (intercalate)

import Data.ByteString (intercalate)

data StatsdMessage = StatsdMessage
    { smName       :: !Name
    , smValue      :: !Int
    , smMetricType :: !MetricType
    , smRate       :: !(Maybe Double)
    , smTags       :: ![Tag]
    } deriving Show

data MetricType = Counter | Gauge | Timer
    deriving Show

type Name = Text
type Tag = (Text,Text)
type Rate = Double

encodeStatsdMessage :: StatsdMessage -> ByteString
encodeStatsdMessage StatsdMessage {..} = intercalate "|" (catMaybes
    [ Just $ encodeUtf8 smName <> ":" <> show smValue
    , Just $ encodeMetricType smMetricType
    , ("@"<>) . show <$> smRate
    ])
    <> encodeTags smTags

encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge   = "g"
encodeMetricType Timer   = "ms"

encodeTags :: [Tag] -> ByteString
encodeTags tags = if null tags then "" else "|#" <> intercalate ","  tagsBSs
  where
    tagsBSs = (\(tagKey,tagVal) -> encodeUtf8 $ tagKey <> (if null tagVal then "" else (":"<>) tagVal))
          <$> tags
