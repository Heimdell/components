
module Db where

import Control.Exception

import Control.Monad.Reader
import Control.Monad.Except

import Component

-- | Examples: Database.
data Db

data DbEnv = DbEnv
    { dbName      :: String
    , dbSomething :: Int
    }

instance IsComponent Db where
    data Error Db
        = NoKey String
        | NoDb  String
        | IOErr IOException
        deriving Show

    type Env Db = DbEnv
    type State Db = ()
    type BaseM Db = IO

    newtype ComponentM Db a = DbComponentM
        { getDbComponentM
            :: ExceptT (Error Db)
            (  ReaderT (Env  Db)
            (  BaseM Db
            )) a
        }
        deriving (Functor, Applicative, Monad, MonadIO, MonadError (Error Db))

    runComponentM  = runERT . getDbComponentM
      where
        runERT action env st = do
            res <- runExceptT action `runReaderT` env
                `catch` (return . Left . IOErr)
            return (res, st)

    catchFrom = catchError

deriving instance MonadReader DbEnv (ComponentM Db)

-- | Sorry, database is out for dinner.
query :: HasComponent (Find Db box) Db box => String -> ComponentM box ()
query msg = do
    select @Db $ do
        throwError $ NoKey msg

