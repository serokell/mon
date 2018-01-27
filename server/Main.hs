{-# LANGUAGE RecordWildCards #-}

-- | Local and non-persistent monitoring server.
-- Listens for UDP statsd messages, parses and stores metrics/events
-- to ekg store whose data is diplayed by ekg-wai http server.
module Main
       ( main
       ) where

import Universum hiding (empty, intercalate)

import Data.Map (empty, insert, member, (!))
import Data.Text (intercalate)
import Network.Socket (AddrInfoFlag (AI_PASSIVE), Socket, SocketType (Datagram), addrAddress,
                       addrFamily, addrFlags, bind, defaultHints, defaultProtocol, getAddrInfo,
                       withSocketsDo)
import Network.Socket.ByteString (recvFrom)
import Options.Applicative (Parser, argument, auto, execParser, fullDesc, fullDesc, helper, info,
                            metavar, progDesc)
import System.Metrics (Store, createCounter, createDistribution, createGauge)
import System.Remote.Monitoring.Wai (forkServer, serverMetricStore)

import Mon.Network (withSocket)
import Mon.Network.Statsd (StatsdMessage (..))
import Mon.Network.Statsd.Parse (decodeStatsdMessage)
import Mon.Types (MetricType (..), Name, Tag)

import qualified System.Metrics.Counter as SMC
import qualified System.Metrics.Distribution as SMD
import qualified System.Metrics.Gauge as SMG

data Metric = CounterM !SMC.Counter
            | GaugeM !SMG.Gauge
            | DistributionM !SMD.Distribution

addValueToMetric :: Int -> Metric -> IO ()
addValueToMetric value (CounterM counter) =
    SMC.add counter $ fromIntegral value
addValueToMetric value (GaugeM gauge) =
    SMG.set gauge $ fromIntegral value
addValueToMetric value (DistributionM distriution) =
    SMD.add distriution $ fromIntegral value

type StoreMap = Map Name Metric

createMetric :: StatsdMessage -> Store -> IO Metric
createMetric StatsdMessage {..} store  = case smMetricType of
    Counter -> CounterM      <$> createCounter      taggedName store
    Gauge   -> GaugeM        <$> createGauge        taggedName store
    Timer   -> DistributionM <$> createDistribution taggedName store
  where
    taggedName = tagName smName smTags

tagName :: Name -> [Tag] -> Text
tagName name tags = name <> intercalate ";" (map showTag tags)
  where
    showTag :: (Text, Text) -> Text
    showTag (tag, value) = tag <> if null value then "" else "=" <> value

addMeasurement :: StatsdMessage -> Store -> StoreMap -> IO StoreMap
addMeasurement statsdMessage@StatsdMessage {..} store storeMap = do
    newMetric <- if member taggedName storeMap
                 then return (storeMap ! taggedName)
                 else createMetric statsdMessage store
    addValueToMetric smValue newMetric
    return $ insert taggedName newMetric storeMap
  where
    taggedName = tagName smName smTags


mainArgsP :: Parser (Int, Int)
mainArgsP = (,)
        <$> argument auto (metavar "STATSD_PORT")
        <*> argument auto (metavar "HTTP_PORT")

main :: IO ()
main = do
    let opts = info (mainArgsP <**> helper) (fullDesc <> progDesc "ekg-based mon server")
    (listenPort, waiPort) <- execParser opts

    store <- serverMetricStore <$> forkServer "127.0.0.1" waiPort
    listen listenPort store

listen :: Int -> Store -> IO ()
listen port store = withSocketsDo $ do
    let hints = defaultHints { addrFlags = [AI_PASSIVE] }
    (sa :_ ) <- getAddrInfo (Just hints)
                            Nothing
                            (Just $ show port)

    withSocket (addrFamily sa) Datagram defaultProtocol $ \sock ->
        bind sock (addrAddress sa) *> handler sock
  where
    handler :: Socket -> IO ()
    handler sock = evaluatingStateT empty . forever $ do
        (msg, _) <- liftIO $ recvFrom sock 1024
        whenRight (decodeStatsdMessage msg) $ \statsdMessage -> do
            print statsdMessage
            storeMap <- get
            newStoreMap <- liftIO $ addMeasurement statsdMessage store storeMap
            put newStoreMap
