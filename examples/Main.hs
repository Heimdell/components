
import Component (Also, runComponentM, catchFrom)

import Db (Db, DbEnv(..), query)
import Logger (Logger, LoggerEnv(..), info)

-- | Whole-box
type App = Db `Also` Logger

config =
    ( DbEnv { dbName = "foo-db", dbSomething = 5 }
    , LoggerEnv { logOutput = "stdout", logSeverity = 0 }
    )

main = print =<< runComponentM @App action config ((), ())
  where
    -- Usage. Notice: all monad stack are concrete; specialise if you want!
    action = do
        info "Logger online"
        do
            query "foo"
            info "Shouldn't be printed"
          `catchFrom` \e ->
            info "Catched!"
