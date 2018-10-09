
# Components

This package is for dividing your app onto components and algebraically composing them,
while not caring about global state or environment or the-type-for-all-errors-combined.

Global state, environment and errors will be composed for you by the combinators provided.

# Reason

Often in Haskell code we see the following:
```haskell
type App = RIO Env IO
```
where `Env` is every possible piece of state/environment clumped together.
Also, sometimes in high-level functions people do amazingly low-level things.

We have to deal with it.

# How

**Tl;dr:** There are leaf components, they do the job. There are compound components, they delegate work to leaves and can't do anything themselves.
The global state and environment is constructed for you. You can catch error from any component you want, leaving any other error to fallthrough.

---

As follows from the name, the main entity to model is _a component_.

_A component_ is a collection of highly-cohesive pieces of functionality.
Examples: Database, Logging.

A component can only do one thing, and do it well.

Each component has its own monad, environment, state and error types.

Component can be constructed from scratch, encapsulating some facet of your application.

You can [Also](./src/Component/Find.hs) construct components out of other components using:
```haskell
type DbWithLogs = Db `Also` Logger `Also` End
```
Do not forget: in that case inner parts of the `DbWithLogs` are **intensionally** invisible and you have
to write all the endpoints for `DbWithLogs` component, constructing them out of endpoints from
inner parts, like that:
```haskell
queryWithLogs :: Contains box DbWithLogs => String -> ComponentM box a
queryWithLogs text = select @DbWithLogs $ do
    info $ "querying: " ++ text  -- from Logger
    query text                   -- from Db
    
```
Since `ComponentM (Also a b)` **intentionally** only implements `Monad`,
you can't do anything inside it but call subcomponents to do their job.

Only leaf components can implement `MonadError`, `MonadReader`, `MonadState` and `MonadIO`.

This is done to stop developers from breaking through abstraction layers. Remember: delegate.

# Example

Imagine, you have 2 components, `Db` and `Logger`:

```haskell
module Logger where

data Logger

instance IsComponent Logger where
    ...
    
info :: Contains box Logger => String -> ComponentM box ()
info msg = do
    select @Logger $ do
        ... some printing there

```

```haskell
module Db where

data Db

instance IsComponent Db where
    ...
    
query :: Contains box Db => String -> ComponentM box ()
info msg = do
    select @Db $ do
        ... some database querying
```

Then, you can do:

```haskell

import Component

import Db
import Logger

type App m = Db `Also` Logger `Also` NoComponent m

main = do
    ...
    runComponentM @(App IO) action config initialState
    ...
  where
    action = do
        info "Logger online"
        do
            query "foo"
            info "Shouldn't be printed"
          `catchFrom` \(e :: Error Db) ->
            info "Catched db error!"
    
    config = (dbConf, (loggerConf, ()))
    
    initialState = (dbState, (loggerState, ()))
```

The `e` you catched will be an `Error Db` from `Db` component.
If logger fails on `info "Shouldn't be printed"`, it will fallthrough `catchFrom` wrapper.

# Caveats

Current implementation is dumb, and unable to see if component is used twice: its state and environment would be duplicated.
This can be seen as feature for some cases with component state; but the duplication of _readonly_ environments is unnessessary.

I may fix this later, possibly by adding `Unique` wrapper.
