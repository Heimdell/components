
module Component.Class
    ( -- * Class
      IsComponent(..)
    )
    where

-- | Abstracts away component capabilities.
class Monad (BaseM comp) => IsComponent comp where
    data Error      comp :: *
    type Env        comp :: *
    type State      comp :: *
    type BaseM      comp :: * -> *
    data ComponentM comp :: * -> *

    -- | Embed the component into its "final" form.
    runComponentM
        :: ComponentM comp a
        -> Env   comp
        -> State comp
        -> BaseM comp
            ( Either (Error comp) a
            , State comp
            )

    -- | Catch an error, originating from the component.
    catchFrom
        :: ComponentM comp a
        -> (Error comp -> ComponentM comp a)
        -> ComponentM comp a
