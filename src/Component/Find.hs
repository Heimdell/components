
module Component.Find where

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
data Also a b

infixr 5 `Also`

data End (m :: * -> *)

-- | IsComponent search path.
data Here
data There r

-- | IsComponent search.
type family Find a b where
    Find a (Also a _) = Here
    Find a (Also _ r) = There (Find a r)
