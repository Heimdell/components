
module Component.List
    (
      -- * Composer
      Also

      -- * Initial empty box
    , End

      -- * Subcomponent access
    , select

      -- * Error picker
    , catchFrom
    , tryThe

      -- * Existence of subcomponent
    , Contains
    ) where

import Control.Monad.Except

import Component.Class (IsComponent(..))
import Component.Monad.Final (ERST, eRST, runERST)
import Component.Find

instance Monad m => IsComponent (End m) where
    data Error (End m)
        deriving Show

    type Env   (End m) = ()
    type State (End m) = ()
    type BaseM (End m) = m

    newtype ComponentM (End m) a = ComponentM0 { getComponentM0 :: m a }

    runComponentM  = runIT . getComponentM0
      where
        runIT action _ st = do
            res <- action
            return (Right res, st)

deriving instance
    (Monad m)
        =>
    (Functor (ComponentM (End m)))

deriving instance
    (Monad m)
        =>
    (Applicative (ComponentM (End m)))

deriving instance
    (Monad m)
        =>
    (Monad (ComponentM (End m)))

instance
    (IsComponent comp, IsComponent other, BaseM comp ~ BaseM other)
        =>
    IsComponent (Also comp other)
  where
    data Error (Also comp other)
        = This (Error comp)
        | That (Error other)

    -- | I think, these can be done better.
    --   It should be possible to eliminame (* 1) type-ops here.
    type Env   (Also comp other) = (Env   comp, Env   other)
    type State (Also comp other) = (State comp, State other)
    type BaseM (Also comp other) =  BaseM comp

    newtype ComponentM (Also comp other) a = ComponentM
        { getComponentM :: ERST
            (Error (Also comp other))
            (Env   (Also comp other))
            (State (Also comp other))
            (BaseM (Also comp other))
             a
        }

    runComponentM  = runERST . getComponentM

-- Why does GHC ignores class constraints when deriving for newtype of type family?
deriving instance
    (Show (Error comp), Show (Error other), IsComponent (Also comp other))
        =>
    (Show (Error (Also comp other)))

deriving instance
    (IsComponent (Also comp other))
        =>
    (Functor (ComponentM (Also comp other)))

deriving instance
    (IsComponent (Also comp other))
        =>
    (Applicative (ComponentM (Also comp other)))

deriving instance
    (IsComponent (Also comp other))
        =>
    (Monad (ComponentM (Also comp other)))

-- | Check if one component has another one as subcomponent
--   and provide access.
class
    (IsComponent comp, IsComponent box, BaseM comp ~ BaseM box)
        =>
    HasComponent path comp box
  where
    selectComponent :: ComponentM comp a -> ComponentM box a
    selectError     :: Error box -> Maybe (Error comp)
    uncoverError    :: ComponentM box a -> ComponentM box (Either (Error comp) a)

-- | Extraction of head component.
instance
    (IsComponent comp, IsComponent other, BaseM comp ~ BaseM other)
        =>
    HasComponent Here comp (Also comp other)
  where
    selectComponent comp = ComponentM . eRST $ \(env, _) (st, st') -> do
        (it, st1) <- runComponentM comp env st
        let
          res = case it of
            Left err -> Left (This err)
            Right a  -> Right a
        return (res, (st1, st'))

    selectError (This err) = Just err
    selectError  _         = Nothing

    uncoverError comp = ComponentM . eRST $ \env st -> do
        (it, st1) <- runComponentM @(Also comp other) comp env st
        let
          res = case it of
            Left err -> case selectError @Here @comp err of
                Nothing  -> Left err
                Just err -> Right (Left err)
            Right a  -> Right (Right a)
        return (res, st1)

-- | Extraction of component from tail.
instance
    (HasComponent path comp rest, BaseM comp' ~ BaseM rest, IsComponent comp', IsComponent rest)
        =>
    HasComponent (There path) comp (Also comp' rest)
  where
    selectComponent comp = ComponentM . eRST $ \(_, env) (st', st) -> do
        (it, st1) <- runComponentM (selectComponent @path comp) env st
        let
          res = case it of
            Left err -> Left (That err)
            Right a  -> Right a
        return (res, (st', st1))

    selectError (That err) = Just err >>= selectError @path
    selectError  _         = Nothing

    uncoverError comp = ComponentM . eRST $ \env st -> do
        (it, st1) <- runComponentM @(Also comp' rest) comp env st
        let
          res = case it of
            Left err -> case selectError @(There path) @comp err of
                Nothing  -> Left err
                Just err -> Right (Left err)
            Right a  -> Right (Right a)
        return (res, st1)

type Contains box comp = HasComponent (Find comp box) comp box

-- | If box has a component, access it.
--
--   When 'comp' and 'box' are known, its possible to 'specialise' this function.
--
select :: forall comp box a . Contains box comp => ComponentM comp a -> ComponentM box a
select = selectComponent @(Find comp box)

-- | Picks errors from 'comp' component of the 'box' specifically.
catchFrom
    :: forall comp box a
    .  Contains box comp
    => ComponentM box a
    -> (Error comp -> ComponentM box a)
    -> ComponentM box a
catchFrom comp handle = do
    uncoverError @(Find comp box) comp >>= either handle return

-- | Analogue to 'Control.Exception.try'
tryThe
    :: forall comp box a
    . Contains box comp
    => ComponentM box a
    -> ComponentM box (Either (Error comp) a)
tryThe comp =
    catchFrom @comp (Right <$> comp) (return . Left)
