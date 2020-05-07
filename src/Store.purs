module React.Basic.Hooks.Store (Instance, Spec, Store, UseStore, useStore) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Bitraversable (ltraverse)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, kill) as AVar
import Effect.Aff (Aff, Error, attempt, error, forkAff, launchAff_, supervise)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseEffect, UseLazy, UseState)
import React.Basic.Hooks as React

-- | A stores internal interface to itself, only accessible inside the `update` function.
type Instance props state m
  = { props :: props
    , readProps :: m props
    , readState :: m state
    , setState :: (state -> state) -> m Unit
    , state :: state
    }

-- | The spec required to configure and run a store.
type Spec props state action m
  = { props :: props
    , init :: state
    , update :: Instance props state m -> action -> m Unit
    , launch :: m Unit -> Aff Unit
    }

-- | A stores external interface, returned from `useStore`.
type Store state action
  = { state :: state
    , readState :: Effect state
    , dispatch :: action -> Effect Unit
    }

newtype UseStore props state action hooks
  = UseStore
  ( UseEffect Unit
      ( UseState (Store state action)
          ( UseEffect Unit
              ( UseLazy Unit (AVar action)
                  ( UseLazy Unit (Ref state)
                      ( UseLazy Unit (Ref props)
                          hooks
                      )
                  )
              )
          )
      )
  )

derive instance newtypeUseStore :: Newtype (UseStore props state action hooks) _

useStore ::
  forall m props state action.
  MonadEffect m =>
  Spec props state action m ->
  Hook (UseStore props state action) (Store state action)
useStore { props, init, update, launch } =
  React.coerceHook React.do
    propsRef <- useUnsafe do Ref.new props
    -- Internal mutable state for fast reads that don't need to touch React state
    stateRef <- useUnsafe do Ref.new init
    -- A variable so the main store loop can subscribe to asynchronous actions sent from the component
    actionBus <- useUnsafe do AVar.empty
    React.useEffectAlways do
      Ref.write props propsRef
      mempty
    store /\ modifyStore <-
      React.useState
        { state: init
        , readState: Ref.read stateRef
        , dispatch:
            -- sends actions to the bus asynchronously
            \action -> launchAff_ do attempt do AVar.put action actionBus
        }
    React.useEffectOnce do
      let
        readProps = liftEffect do Ref.read propsRef

        readState = liftEffect do Ref.read stateRef

        setState f =
          liftEffect do
            state' <- Ref.modify f stateRef
            modifyStore _ { state = state' }
      -- This is the main loop. It waits for an action to come in over the bus and then runs the `update` function from
      -- the spec in a forked fiber. State updates are applied to the local mutable state and pushed back to React for
      -- rendering. This continues until the action bus is shut down, causing the main loop to terminate and all child
      -- fibers to be cleaned up.
      -- - `forever` will cause it to loop indefinitely
      -- - `supervise` will clean up forked child fibers when the main fiber is shutdown
      -- - `attempt` will prevent the shutdown from logging an error
      (launchAff_ <<< attempt <<< supervise <<< forever) do
        action <- AVar.take actionBus
        -- We log these errors because they are created by the `update` function
        (forkAff <<< logError <<< attempt) do
          currentProps <- liftEffect do Ref.read propsRef
          currentState <- liftEffect do Ref.read stateRef
          let
            store' =
              { props: currentProps
              , readProps
              , readState
              , setState
              , state: currentState
              }
          launch do update store' action
      pure do
        -- When the component unmounts, trigger the main loop shutdown by killing the action bus.
        AVar.kill (error "Store action bus killed") actionBus
    pure store

useUnsafe :: forall a. Effect a -> Hook (UseLazy Unit a) a
useUnsafe effect = React.useLazy unit \_ -> unsafePerformEffect effect

logError :: forall m a. MonadEffect m => m (Either Error a) -> m Unit
logError ma = void $ ma >>= ltraverse Console.errorShow
