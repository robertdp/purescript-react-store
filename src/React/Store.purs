module React.Store where

import Prelude
import Control.Monad.Rec.Class (forever)
import Control.Monad.Resource as Resource
import Control.Monad.Trans.Class (lift)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AffAVar
import Effect.Aff.AVar as AffVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Store.Internal (ComponentM, Lifecycle(..), evalComponent)
import Unsafe.Reference (unsafeRefEq)

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
        { dispatch: \action -> Aff.launchAff_ do Aff.attempt do AffVar.put (Action action) eventQueue
        , state: init
        }
    React.useEffectAlways do
      props' <- Ref.read propsRef
      unless (unsafeRefEq props props') do
        Ref.write props propsRef
        Aff.launchAff_ $ Aff.attempt $ AffVar.put (Update props') eventQueue
      mempty
    React.useEffectOnce do
      let
        runStore event = do
          state <- liftEffect $ Ref.read stateRef
          evalComponent stateRef store.dispatch (eval event)
          state' <- liftEffect $ Ref.read stateRef
          unless (unsafeRefEq state state') do
            liftEffect $ modifyStore _ { state = state' }
      triggerCleanup <- AVar.empty
      (Aff.launchAff_ <<< Resource.runResource) do
        runStore (Initialize props)
        fiber <- Resource.fork $ forever $ runStore =<< lift (AffVar.take eventQueue)
        lift do
          AffAVar.take triggerCleanup
          Aff.killFiber (Aff.error "Finalizing") fiber
          AffAVar.kill (Aff.error "Finalizing") eventQueue
          AffAVar.kill (Aff.error "Finalizing") triggerCleanup
        runStore Finalize
      pure do
        Aff.launchAff_ do Aff.attempt do AffVar.put unit triggerCleanup
    pure (render { props, state: store.state, dispatch: store.dispatch })
