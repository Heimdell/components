
import Component

import Db
import Logger

-- | Whole-box
type App = Db `Also` Logger

main = print =<< runComponentM @App action (("foo", 42), 0.5) ((), ())
  where
    -- Usage. Notice: all monad stack are concrete; specialise if you want!
    action = do
        info "Logger online"
        do
            query "foo"
            info "Shouldn't be printed"
          `catchFrom` \e ->
            info "Catched!"
