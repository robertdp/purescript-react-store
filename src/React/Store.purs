module React.Store where

import Prelude
import Control.Monad.Free (liftF)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Resource (ReleaseKey)
import Control.Monad.Resource as Resource
import Control.Monad.Trans.Class (lift)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AffAVar
import Effect.Aff.AVar as AffVar
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Store.Internal (ComponentF(..), ComponentM(..), EventSource, Lifecycle(..), evalComponent)
import Unsafe.Reference (unsafeRefEq)

subscribe' :: forall state action m. (ReleaseKey -> EventSource action m) -> ComponentM state action m ReleaseKey
subscribe' fes = ComponentM $ liftF $ Subscribe fes identity

subscribe :: forall m action state. EventSource action m -> ComponentM state action m ReleaseKey
subscribe = subscribe' <<< const

release :: forall m action state. ReleaseKey -> ComponentM state action m Unit
release key = ComponentM $ liftF $ Release key unit

fork :: forall m action state. ComponentM state action m Unit -> ComponentM state action m ReleaseKey
fork run = ComponentM $ liftF $ Fork run identity

type Component props state action
  = { init :: state
    , eval :: Lifecycle props action -> ComponentM state action Aff Unit
    , render ::
        { props :: props
        , state :: state
        , dispatch :: action -> Effect Unit
        } ->
        JSX
    }

component ::
  forall state action props.
  String ->
  Component props state action ->
  Effect (props -> JSX)
component name { init, eval, render } =
  React.component name \props -> React.do
    { eventQueue, propsRef, stateRef } <-
      React.useMemo unit \_ ->
        unsafePerformEffect ado
          eventQueue <- AVar.empty
          propsRef <- Ref.new props
          stateRef <- Ref.new init
          in { eventQueue, propsRef, stateRef }
    store /\ modifyStore <-
      React.useState
        { dispatch: \action -> Aff.launchAff_ do AffVar.put (Action action) eventQueue
        , state: init
        }
    React.useEffectAlways do
      props' <- Ref.read propsRef
      unless (unsafeRefEq props props') do
        Ref.write props propsRef
        Aff.launchAff_ $ AffVar.put (Props props') eventQueue
      mempty
    React.useEffectOnce do
      let
        runStore event = do
          evalComponent
            { stateRef
            , render: \state -> modifyStore _ { state = state }
            , enqueueAction: store.dispatch
            }
            (eval event)
      blockUntilUnmount <- AVar.empty
      (Aff.launchAff_ <<< Resource.runResource) do
        runStore (Initialize props)
        fiber <- Resource.fork $ forever $ runStore =<< lift (AffVar.take eventQueue)
        lift do
          AffAVar.take blockUntilUnmount
          Aff.killFiber (Aff.error "Finalizing") fiber
          AffAVar.kill (Aff.error "Finalizing") blockUntilUnmount
        runStore Finalize
      pure do
        Aff.launchAff_ do
          AffAVar.kill (Aff.error "Finalizing") eventQueue
          AffVar.put unit blockUntilUnmount
    pure (render { props, state: store.state, dispatch: store.dispatch })
