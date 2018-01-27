{-# LANGUAGE RecordWildCards #-}

-- | Parsers for the mon wire protocol messages.
module Mon.Network.Statsd.Parse
       ( decodeStatsdMessage
       , statsdMessageP
       ) where

import Universum

import Data.Attoparsec.Text (Parser, char, decimal, double, endOfInput, parseOnly, sepBy1, string,
                             takeWhile1, (<?>))

import Mon.Network.Statsd (StatsdMessage (..))
import Mon.Types (MetricType (..), Tag)


-- | Try to parse a 'ByteString' as a 'StatsdMessage'.
decodeStatsdMessage :: ByteString -> Either Text StatsdMessage
decodeStatsdMessage = first fromString
                    . parseOnly (statsdMessageP <* endOfInput)
                    . decodeUtf8

-- | Parse a 'StatsdMessage'.
statsdMessageP :: Parser StatsdMessage
statsdMessageP = flip (<?>) "statsdMessage" $ do
    smName       <- takeWhile1 (/= ':') <* char ':'
    smValue      <- decimal
    smMetricType <- char '|' *> metricTypeP
    smRate       <- optional (string "|@" *> double)
    smTags       <- string "|#" *> tagsP <|> pure []
    pure StatsdMessage{..}

-- | Parse a 'MetricType'.
metricTypeP :: Parser MetricType
metricTypeP = string "c"  $> Counter
          <|> string "g"  $> Gauge
          <|> string "ms" $> Timer
          <?> "metricType"

-- | Parse a list of 'Tag's.
tagsP :: Parser [Tag]
tagsP = tagP `sepBy1` char ',' <?> "tags"

-- | Parse a single 'Tag'.
tagP :: Parser Tag
tagP = do
    key <- takeWhile1 (`notElem` [':', ','])
    val <- char ':' *> takeWhile1 (/= ',') <|> pure ""
    pure (key, val)
