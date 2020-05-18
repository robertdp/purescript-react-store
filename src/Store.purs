module React.Basic.Hooks.Store
  ( Instance
  , Spec
  , Store
  , UseStore
  , useStore
  , Instance'
  , Spec'
  , UseStore'
  , useStore'
  ) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Bitraversable (ltraverse)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Aff.AVar as AffVar
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

type Instance' state m
  = Instance Unit state m

-- | The spec required to configure and run a store.
type Spec props state action m
  = { props :: props
    , init :: state
    , update :: Instance props state m -> action -> m Unit
    , launch :: m Unit -> Aff Unit
    }

type Spec' state action m
  = { init :: state
    , update :: Instance' state m -> action -> m Unit
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
              ( UseLazy Unit
                  { actionQueue :: AVar action
                  , propsRef :: Ref props
                  , stateRef :: Ref state
                  }
                  hooks
              )
          )
      )
  )

derive instance newtypeUseStore :: Newtype (UseStore props state action hooks) _

type UseStore' state action hooks
  = UseStore Unit state action hooks

useStore ::
  forall m props state action.
  MonadEffect m =>
  Spec props state action m ->
  Hook (UseStore props state action) (Store state action)
useStore { props, init, update, launch } =
  React.coerceHook React.do
    { actionQueue, propsRef, stateRef } <-
      React.useLazy unit \_ ->
        unsafePerformEffect ado
          -- A variable so the main store loop can subscribe to asynchronous actions sent from the component
          actionQueue <- AVar.empty
          -- A mutable version of props that gets constantly updated for access inside `update`
          propsRef <- Ref.new props
          -- Internal mutable state for fast reads that don't need to touch React state
          stateRef <- Ref.new init
          in { actionQueue, propsRef, stateRef }
    React.useEffectAlways do
      Ref.write props propsRef
      mempty
    store /\ modifyStore <-
      React.useState
        { state: init
        , readState: Ref.read stateRef
        , dispatch:
            -- sends actions to the bus asynchronously
            \action -> Aff.launchAff_ do Aff.attempt do AffVar.put action actionQueue
        }
    React.useEffectOnce do
      let
        readProps :: forall n. MonadEffect n => n props
        readProps = liftEffect do Ref.read propsRef

        readState :: forall n. MonadEffect n => n state
        readState = liftEffect do Ref.read stateRef

        setState :: (state -> state) -> m Unit
        setState f =
          liftEffect do
            state <- Ref.modify f stateRef
            modifyStore _ { state = state }
      -- This is the main loop. It waits for an action to come in over the bus and then runs the `update` function from
      -- the spec in a forked fiber. State updates are applied to the local mutable state and pushed back to React for
      -- rendering. This continues until the action bus is shut down, causing the main loop to terminate and all child
      -- fibers to be cleaned up.
      -- - `forever` will cause it to loop indefinitely
      -- - `supervise` will clean up forked child fibers when the main fiber is shutdown
      -- - `attempt` will prevent the shutdown from logging an error
      fiber <-
        (Aff.launchAff <<< Aff.attempt <<< Aff.supervise <<< Rec.forever) do
          action <- AffVar.take actionQueue
          -- We log these errors because they are created by the `update` function
          (Aff.forkAff <<< logError <<< Aff.attempt) do
            currentProps <- readProps
            currentState <- readState
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
        let
          message = Aff.error "Unmounting"
        AVar.kill message actionQueue
        Aff.launchAff_ do Aff.killFiber message fiber
    pure store

useStore' ::
  forall m state action.
  MonadEffect m =>
  Spec' state action m ->
  Hook (UseStore' state action) (Store state action)
useStore' { init, update, launch } = useStore { props: unit, init, update, launch }

logError :: forall m a. MonadEffect m => m (Either Error a) -> m Unit
logError ma = void $ ma >>= ltraverse Console.errorShow
