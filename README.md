# purescript-react-store

This library provides `useStore`, a React hook which uses a simple and convenient approach to state management for components. The `react-basic-hooks` library is required, and some familiarity is assumed.

## Interesting points:

- each store maintains a single dedicated asynchronous fiber
- dispatched actions are handled using a queue
- each action is run in its own fork
- the fiber and queue are cleaned up when the component unmounts
- the state is managed internally for consistency, and pushed to React when changed

## What does this mean?

- actions can be handled in parallel
- long-running asynchronous and parallel computations are supported, along with state updates
- full automatic shutdown of all asynchronous logic when unmounting

### Note

If you're not sure you need `useStore`, try the hooks exposed by `react-basic-hooks` first. Just using `useEffect` and `useReducer` can get you a long way, and that library is even nice enough to include `useAffReducer` for a mini Elm-style approach.

## Continuing on...

Let's start with a basic counter component:

```purs
module Example where

import Prelude
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type CounterProps
  = { start :: Int
    , step :: Int
    }

mkCounter :: Component CounterProps
mkCounter =
  React.component "Counter" \props -> React.do
    count /\ modifyCount <- React.useState props.start
    pure
      $ React.fragment
          [ R.button
              { onClick: handler_ do modifyCount (_ - props.step)
              , children: [ R.text "-" ]
              }
          , R.text (show count)
          , R.button
              { onClick: handler_ do modifyCount (_ + props.step)
              , children: [ R.text "+" ]
              }
          , R.button
              { onClick: handler_ do modifyCount (const props.start)
              , children: [ R.text "Reset" ]
              }
          ]
```

At this scale, there is no problem with this component. But as our components grow their list of interactions also grow, and their state becomes more complex along with it.

So if state and interactions are more complex, how do we run the logic to deal with them? It can be annoying to deal with asynchronous logic using `Effect` via the `useEffect` hook, but on a similar note `useAffReducer` is also a bit basic in its use of `Aff` and requires some degree of fragmentation (as does Elm).

What if we could write state management logic as straightforward reducer in an effectful monad of our choice?

```purs
module Example where

import Prelude
import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import React.Store as Store

type CounterProps
  = { start :: Int
    , step :: Int
    }

data Action
  = Decrement
  | Increment
  | Reset

update ::
  forall m.
  MonadEffect m =>
  Store.Instance CounterProps Int m ->
  Action ->
  m Unit
update self = case _ of
  Decrement -> self.setState (_ - self.props.step)
  Increment -> self.setState (_ + self.props.step)
  Reset -> self.setState (const self.props.start)

mkCounter :: Component CounterProps
mkCounter =
  React.component "Counter" \props -> React.do
    store <-
      Store.useStore
        { props
        , init: props.start
        , update
        , launch: liftEffect
        }
    pure
      $ React.fragment
          [ R.button
              { onClick: handler_ do store.dispatch Decrement
              , children: [ R.text "-" ]
              }
          , R.text (show store.state.count)
          , R.button
              { onClick: handler_ do store.dispatch Increment
              , children: [ R.text "+" ]
              }
          , R.button
              { onClick: handler_ do store.dispatch Reset
              , children: [ R.text "Reset" ]
              }
          ]
```

When using `useStore` we need to give it a spec containing 4 things:

1. the current `props` to make them available inside our update function
2. an `init`ial state
3. our `update` function, which runs our logic in any monad we want
4. a `launch` function, which has to be able to turn our logic monad into an `Aff`

```purs
-- | The spec required to configure and run a store.
type Spec props state action m
  = { props :: props
    , init :: state
    , update :: Instance props state m -> action -> m Unit
    , launch :: m Unit -> Aff Unit
    }
```

Our `update` function gets access to the running store, here called `Instance`, and also gets passed the action that invoked it:

```purs
-- | A stores internal interface, only accessible inside the `update` function.
type Instance props state m
  = { props :: props
    , readProps :: m props
    , readState :: m state
    , setState :: (state -> state) -> m Unit
    , state :: state
    }
```

And with this our component has access to a `Store`:

```purs
-- | A stores external interface, returned from `useStore`.
type Store state action
  = { state :: state
    , readState :: Effect state
    , dispatch :: action -> Effect Unit
    }
```

