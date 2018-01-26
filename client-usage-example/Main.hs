-- | Example use case of Client monitoring library.
module Main
       ( main
       ) where

import Universum

import Control.Concurrent (threadDelay)
import Options.Applicative (Parser, argument, auto, execParser, helper, info, metavar, progDesc,
                            strArgument)

import Mon (recordCounter, recordGauge, recordTimer, reportEvent)

mainArgsP :: Parser (Text,Int)
mainArgsP = (,)
        <$> (fmap toText $ strArgument (metavar "<monitoring_server_host>"))
        <*> argument auto (metavar "<monitoring_server_statsd_listen_port>")

cliMessage :: String
cliMessage = "Before running this example have local-server running with"
          <> "'local-server <statsd_listen_port> <ekg_http_interface_port>'"
          <> " and then run 'client-usage-example 127.0.0.1 "
          <> "<monitoring_server_statsd_listen_port>'"
          <> " or specify host and port of monitoring server."


-- | To run examples, have local or monitoring server running for receiving
--   test calls of client's record/report functions.
main :: IO ()
main = do
    (host,port) <- execParser $ info (helper <*> mainArgsP) $
                   progDesc cliMessage
    let endpoint = (host, port)
    loop endpoint  0
  where
    loop endpoint index = do
        recordCounter endpoint "test.counter" 2 [] 3
        recordGauge endpoint "test.gauge" 2 [] 3
        recordTimer endpoint "test.timer.saw" 2 [] (index `mod` 30)
        recordTimer endpoint "test.timer.linear" 2 [] index
        recordTimer endpoint "test.timer.constant" 2 [] 5
        recordTimer endpoint "test.timer.jumping" 2 []
                    (index * 104743 `mod` 8171)
        reportEvent endpoint "test.event" 2 []
        reportEvent endpoint "test.tags.event" 2
                    [("tag0Key","tag0Val"),("tag1","")]
        threadDelay 100000
        loop endpoint $ succ index



