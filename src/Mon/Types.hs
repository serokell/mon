{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mon.Types
       ( StatsdMessage (..)
       , MetricType (..)
       , Name
       , Tag
       , Rate
       , encodeStatsdMessage
       ) where

import Data.ByteString (intercalate)
import Universum hiding (intercalate)

data StatsdMessage = StatsdMessage
    { smName :: !Name
    , smValue :: !Int
    , smMetricType :: !MetricType
    , smRate :: !(Maybe Double)
    , smTags :: ![(Tag,Maybe Text)]
    }

data MetricType = Counter | Gauge | Timer

type Name = Text
type Tag = Text
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
encodeMetricType Gauge = "g"
encodeMetricType Timer = "ms"

encodeTags :: [(Tag, Maybe Text)] -> ByteString
encodeTags tags = if null tags then "" else "|#" <> intercalate ","  tagsBSs
    where
    tagsBSs = fmap (\(tag,tagValue) -> encodeUtf8 $ tag <> maybe "" (":"<>) tagValue)
                   tags

