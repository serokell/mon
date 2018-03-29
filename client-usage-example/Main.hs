-- | Example use case of Client monitoring library.
module Main
       ( main
       ) where

import Universum

import Control.Concurrent (threadDelay)
import Options.Applicative (Parser, argument, auto, execParser, fullDesc, helper, info, metavar,
                            progDesc, strArgument)

import Mon (recordCounter, recordGauge, recordTimer, reportEvent)
import Mon.Network (Endpoint)

mainArgsP :: Parser (Text, Int)
mainArgsP = (,)
        <$> strArgument (metavar "STATSD_HOST")
        <*> argument auto (metavar "STATSD_PORT")

cliMessage :: String
cliMessage = "Before running this example have local-server running with"
          <> " 'local-server STATSD_PORT HTTP_PORT'"
          <> " and then run 'client-usage-example 127.0.0.1 STATSD_PORT'."

threadDelayPeriod :: Int
threadDelayPeriod = 1000 * 1000
-- | To run examples, have local or monitoring server running for receiving
-- test calls of client's record/report functions.
main :: IO ()
main = do
    let opts = info (mainArgsP <**> helper) (fullDesc <> progDesc cliMessage)
    endpoint <- execParser opts
    evaluatingStateT (0::Int) . forever $ do
        get >>= clientAction endpoint
        modify succ
  where
    clientAction :: (MonadIO m) => Endpoint -> Int -> m ()
    clientAction endpoint index = liftIO $ do
        recordCounter endpoint "test.counter.const" 1 [] 1
        recordGauge endpoint "test.gauge.const" 1 [] 1
        recordTimer endpoint "test.timer.saw" 1 [] (index `mod` 30)
        recordTimer endpoint "test.timer.linear" 1 [] index
        recordTimer endpoint "test.timer.constant" 1 [] 5
        recordTimer endpoint "test.timer.jumping" 1 []
                    (index * 104743 `mod` 8171)
        reportEvent endpoint "test.event" 1 []
        reportEvent endpoint "test.tags.event" 1
                    [("tag0Key","tag0Val"),("tag1","")]
        threadDelay threadDelayPeriod




