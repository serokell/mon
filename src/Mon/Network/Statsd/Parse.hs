-- | Parsers for the mon wire protocol messages.
module Mon.Network.Statsd.Parse
       ( decodeStatsdMessage
       , statsdMessageP
       ) where

import Universum hiding (takeWhile)

import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, double, sepBy, string, takeWhile,
                                         (<?>))

import Mon.Network.Statsd (StatsdMessage (..))
import Mon.Types (MetricType (..), Tag)


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
