module React.Basic.Hooks.Store where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Bitraversable (ltraverse)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.AVar (empty, kill)
import Effect.Aff (Aff, Error, attempt, error, forkAff, launchAff_, supervise)
import Effect.Aff.AVar (put, take)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (modify, new, read)
import Effect.Ref as Ref
import React.Basic.Hooks (Hook, UseEffect, UseState)
import React.Basic.Hooks as Hooks

-- | A stores internal interface to itself, only accessible inside the `update` function.
type Instance state m
  = { setState :: (state -> state) -> m Unit
    , state :: state
    , readState :: m state
    }

type Dispatch action
  = action -> Effect Unit

-- | The spec required to configure and run a store.
type Spec state action m
  = { init :: state
    , update :: Instance state m -> action -> m Unit
    , launch :: m Unit -> Aff Unit
    }

-- | A stores external interface, returned from `useStore`.
type Store state action
  = { state :: state
    , readState :: Effect state
    , dispatch :: action -> Effect Unit
    , initialised :: Boolean
    }

newtype UseStore state action hooks
  = UseStore
  ( UseEffect Unit
      ( UseState (Store state action) hooks
      )
  )

derive instance newtypeUseStore :: Newtype (UseStore state action hooks) _

useStore ::
  forall state action m.
  MonadEffect m =>
  Spec state action m ->
  Hook (UseStore state action) (Store state action)
useStore { init, update, launch } = do
  Hooks.coerceHook Hooks.do
    store /\ setStore <-
      Hooks.useState
        { state: init
        , readState: pure init
        , dispatch: \_ -> Console.warn "Dispatch to uninitialised store"
        , initialised: false
        }
    Hooks.useEffect unit do
      -- Internal mutable state for fast reads that don't need to touch React state
      stateRef <- new init
      -- A variable so the main store loop can subscribe to asynchronous actions sent from the component
      actionBus <- empty
      setStore
        _
          { readState = read stateRef
          , dispatch =
            -- sends actions to the bus asynchronously
            \action -> launchAff_ do attempt do put action actionBus
          , initialised = true
          }
      let
        setState f =
          liftEffect do
            newState <- modify f stateRef
            setStore _ { state = newState }
      -- This is the main loop. It waits for an action to come in over the bus and then runs the `update` function from
      -- the spec in a forked fiber. State updates are applied to the local mutable state and pushed back to React for
      -- rendering. This continues until the action bus is shut down, causing the main loop to terminate and all child
      -- fibers to be cleaned up.
      -- - `forever` will cause it to loop indefinitely
      -- - `supervise` will clean up forked child fibers when the main fiber is shutdown
      -- - `attempt` will prevent the shutdown from logging an error
      (launchAff_ <<< attempt <<< supervise <<< forever) do
        action <- take actionBus
        -- We log these errors because they are created by the `update` function
        (forkAff <<< logError <<< attempt) do
          currentState <- liftEffect $ read stateRef
          let
            store' =
              { readState: liftEffect $ read stateRef
              , setState
              , state: currentState
              }
          launch do update store' action
      pure do
        -- When the component unmounts, trigger the main loop shutdown by killing the action bus.
        kill (error "Store action bus killed") actionBus
    pure store

logError :: forall m a. MonadEffect m => m (Either Error a) -> m Unit
logError ma = void $ ma >>= ltraverse Console.errorShow

-- | A helper for `Hooks.useEffect` that waits for a `Store` to finish initialising before running effects.
useEffect ::
  forall state action key.
  Eq key =>
  Store state action ->
  key ->
  Effect (Effect Unit) ->
  Hook (UseEffect (Boolean /\ key)) Unit
useEffect store key eff = do
  Hooks.useEffect (store.initialised /\ key) do
    cleanup <- Ref.new mempty
    when store.initialised do
      eff >>= flip Ref.write cleanup
    Ref.read cleanup
