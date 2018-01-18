{-# LANGUAGE RecordWildCards #-}

-- | StatsdMessage is a representation of a statsd udp message
--   This module also contains functions for parsing/encoding
--   StatsdMessage from/to ByteString in which fromat it is received/sent
--   by Server/Client

module Mon.Types
       ( StatsdMessage (..)
       , MetricType (..)
       , Name
       , Tag
       , Rate
       , encodeStatsdMessage
       , decodeStatsdMessage
       ) where

import Universum hiding (intercalate, takeWhile)

import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, double, sepBy, string, takeWhile,
                                         (<?>))
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
encodeStatsdMessage StatsdMessage {..} = intercalate "|" $ catMaybes
    [ Just $ encodeUtf8 smName <> ":" <> show smValue
    , Just $ encodeMetricType smMetricType
    , ("@" <>) . show <$> smRate
    , ("#" <>) <$> encodeTags smTags
    ]

encodeMetricType :: MetricType -> ByteString
encodeMetricType Counter = "c"
encodeMetricType Gauge   = "g"
encodeMetricType Timer   = "ms"

encodeTags :: [Tag] -> Maybe ByteString
encodeTags []   = Nothing
encodeTags tags = Just $ intercalate "," (map encodeTag tags)
  where
    encodeTag (tag, val) = encodeUtf8 $ tag <> if null val then "" else ":" <> val


statsdMessageP :: Parser StatsdMessage
statsdMessageP = flip (<?>) "statsdMessage" $ do
    name <- takeWhile (/= ':')
    void colonP
    value <- decimal
    void pipeP
    metricType <- metricTypeP
    maybeRate <- optional $ pipeP >> atP >> double
    maybeTags <- optional $ pipeP >> hashP >> tagsP
    return $ StatsdMessage
        { smName = decodeUtf8 name
        , smValue = value
        , smMetricType = metricType
        , smRate = maybeRate
        , smTags = fromMaybe [] maybeTags
        }

pipeP, colonP, atP, hashP, commaP :: Parser Char
[pipeP, colonP, atP, hashP, commaP] = char <$> "|:@#,"

tagsP :: Parser [Tag]
tagsP = tagP `sepBy` commaP <?> "tags"

tagP, tagKeyP, tagKeyValueP :: Parser Tag
tagP = tagKeyValueP <|> tagKeyP <?> "tag"
tagKeyP = flip (<?>) "tagKey" $ do
    key <- takeWhile (/= ',')
    return (decodeUtf8 key, "")

tagKeyValueP = flip (<?>) "tagKeyValue" $ do
    tag <- takeWhile ((&&) <$> (/= ':') <*> (/= ','))
    void colonP
    tagValue <- takeWhile (/= ',')
    return (decodeUtf8 tag, decodeUtf8 tagValue)

metricTypeP :: Parser MetricType
metricTypeP =
    (string "c"  >> return Counter)   <|>
    (string "g"  >> return Gauge)     <|>
    (string "ms" >> return Timer)
    <?> "metricType"

decodeStatsdMessage :: ByteString -> StatsdMessage
decodeStatsdMessage bs = case parseOnly statsdMessageP bs of
    Left err -> error $ toText err
    Right sm -> sm
