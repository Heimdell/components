
module Logger where

import Control.Monad.Reader

import Component

-- | A logger.
data Logger

data LoggerEnv = LoggerEnv
    { logOutput   :: String
    , logSeverity :: Int
    }

instance IsComponent Logger where
    data Error Logger
        deriving Show

    type Env   Logger = LoggerEnv
    type State Logger = ()
    type BaseM Logger = IO

    newtype ComponentM Logger a = LogComponentM
        { getLogComponentM
            :: ReaderT (Env Logger)
              (BaseM Logger)
               a
        }
        deriving (Functor, Applicative, Monad, MonadIO)

    runComponentM  = runRT . getLogComponentM
      where
        runRT action env st = do
            res <- runReaderT action env
            return (Right res, st)

    catchFrom action _ = action

deriving instance MonadReader LoggerEnv (ComponentM Logger)

info :: HasComponent (Find Logger box) Logger box => String -> ComponentM box ()
info msg = do
    select @Logger $ do
        LoggerEnv logOutput logSeverity <- ask
        when (logSeverity < 1) $ do
            liftIO $ putStrLn $ "INFO (to: " ++ logOutput ++ "): " ++ msg
