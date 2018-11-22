# Mon

Mon is a lightweight library to emit metrics using the StatsD protocol.

Purpose of the library is to achieve monitoring functionality by adding minimal weight on the code being monitored e.g. small set of dependencies and minimal code integration. This is accomplished with simple `MonadIO` functions that send metrics to the receiving server.

Library also contains a server which can be run locally in order to avoid setting up a real (remote) monitoring server.

Project is described in more detail in this [document](https://docs.google.com/document/d/1Sa-P58foIBnzzO7-OBRwGP63kX_GveiO824_3Fd2WBU/edit#heading=h.kraf7dd0j1m).

Simple examples of using a client can be found [here](https://github.com/serokell/mon/blob/master/client-usage-example/Main.hs).

## Build instructions [↑](#-mon)

### Client
Add `mon` library to the cabal project dependencies.

### Server
`stack install` installs `local-server` binary used for local testing of a library.
Beside server, stack also installs  `client-usage-example` demo .

## Usage [↑](#-mon)

Call appropriate [record\* function](https://github.com/serokell/mon/blob/master/src/Mon.hs#L3) from your `MonadIO` code that you wish to monitor.
Test this calls on `localhost` by running `local-server` with the corresponding port.
### Example
Start a server:

`local-server 8125 8888`

Run demo that periodically calls record\* [examples](https://github.com/serokell/mon/blob/master/client-usage-example/Main.hs):

`client-usage-example localhost 8125`

See incoming metrics in `local-server` call and graphs on a ekg server at `localhost:8888`.
## For Contributors [↑](#-mon)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## About Serokell [↑](#-mon)

Mon is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See [our other
projects](https://serokell.io/community?utm_source=github) or [hire
us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow
your idea!
