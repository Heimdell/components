
# Components

This package is for dividing your app onto components and algebraically composing them,
while not caring about global state or environment or type-for-all-errors-combined.

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

