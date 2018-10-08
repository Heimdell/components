
module Component.Monad.Final (ERST, eRST, runERST) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader

-- | The final component monad.
type ERST e r s m = ExceptT e (StateT s (ReaderT r m))

runERST :: ERST e r s m a -> r -> s -> m (Either e a, s)
runERST action r s = runReaderT (runExceptT action `runStateT` s) r

eRST :: (r -> s -> m (Either e a, s)) -> ERST e r s m a
eRST action = ExceptT $ StateT $ \s -> ReaderT $ \r -> action r s
