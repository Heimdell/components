
module Component.List
    (
      -- * Composer
      Also

      -- * Subcomponent access
    , select

      -- * Existence of subcomponent
    , Contains
    ) where

import Control.Monad.Except

import Component.Class (IsComponent(..))
import Component.Monad.Final (ERST, eRST, runERST)

-- | Dumb naive component composer.
--
--   Does its work by making appropriate tensor products over
--   subcomponents parts.
--
--   So, if 'State comp' is 'Foo' and 'State other' is 'Bar',
--   the 'State (comp `Also` bar)' is '(Foo, Bar)' - a product.
--
--   The errors are summed. If your component can't throw, make its
--   'Error' isomorphic to 'Data.Void.Void'.
--
--   Its 'ComponentM' deliberately lacks any MonadReader, State, Writer or IO
--   instances. You can't access global context, state, make I/O, etc.
--   You should do it through subcomponents.
--
--   Usage: if 'comp' and 'other' are components with same 'BaseM',
--          then so does 'comp `Also` other'. It infers its 'State', 'Env', 'Error'
--          and all methods.
--
data comp `Also` box

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

    catchFrom action handler =
        ComponentM $
            getComponentM action
                `catchError` (getComponentM . handler)

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

-- | IsComponent search path.
data Here
data There r
data Itself

-- | IsComponent search.
type family Find a b where
    Find a (Also a _) = Here
    Find a (Also _ r) = There (Find a r)
    Find a  a         = Itself

-- | Extraction of head component.
instance
    (IsComponent comp, IsComponent other, BaseM comp ~ BaseM other, comp ~ comp')
        =>
    HasComponent Here comp (Also comp' other)
  where
    selectComponent comp = ComponentM . eRST $ \(env, _) (st, st') -> do
        (it, st1) <- runComponentM comp env st
        let
          res = case it of
            Left err -> Left (This err)
            Right a  -> Right a
        return (res, (st1, st'))

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

-- | Extraction of component from itself.
instance
    IsComponent comp
        =>
    HasComponent Itself comp comp
  where
    selectComponent = id

type Contains box comp = HasComponent (Find comp box) comp box

-- | If box has a component, access it.
--
--   When 'comp' and 'box' are known, its possible to 'specialise' this function.
--
select :: forall comp box a . Contains box comp => ComponentM comp a -> ComponentM box a
select = selectComponent @(Find comp box)
