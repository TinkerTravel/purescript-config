# purescript-config

Applicative configuration inspired by the talk
[Move Over Free Monads: Make Way for Free Applicatives!][talk]

[talk]: https://www.youtube.com/watch?v=H28QqxO7Ihc

## Example

You can use the applicative DSL in `Data.Config` to build a description of your
configuration. This description contains the keys and types of your
configuration, for consumption by various _interpreters_. Here is an example of
such a description, for PostgreSQL connections:

```purescript
import Database.PostgreSQL (PoolConfiguration)

postgreSQLPool :: Config {name :: String} PoolConfiguration
postgreSQLPool =
  { user: _, password: _, host: _, port: _, database: _
  , max: 10, idleTimeoutMillis: 0 }
  <$> string {name: "user"}
  <*> string {name: "password"}
  <*> string {name: "host"}
  <*> int    {name: "port"}
  <*> string {name: "database"}
```

## Interpreters

Currently one interpreter is provided: one that reads environment variables.
This interpreter is called `fromEnv` and can be found in `Data.Config.Node`:

```purescript
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Config.Node (fromEnv)

main :: ∀ eff. Eff (process :: PROCESS | eff) Unit
main = do
  config <- fromEnv "DB" postgreSQLPool
  ...
```

When applied to the above description, it will read these environment variables,
derived from the `name` fields in the records in the description:
`DB_USER`, `DB_PASSWORD`, `DB_HOST`, `DB_PORT`, `DB_DATABASE`. It returns a set
of incorrect variables (`Left`), or the complete configuration (`Right`).
