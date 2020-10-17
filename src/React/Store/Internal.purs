module React.Store.Internal where

import Prelude
import Control.Applicative.Free (FreeAp, hoistFreeAp, retractFreeAp)
import Control.Monad.Free (Free, hoistFree, liftF, runFreeM)
import Control.Monad.Resource (ReleaseKey, Resource)
import Control.Monad.Resource as Resource
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (lmap)
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, parallel, sequential)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Unsafe.Reference (unsafeRefEq)

newtype EventSource a m
  = EventSource ((a -> Effect Unit) -> m (m Unit))

data ComponentF state action m a
  = State (state -> Tuple a state)
  | Subscribe (ReleaseKey -> EventSource action m) (ReleaseKey -> a)
  | Release ReleaseKey a
  | Lift (m a)
  | Par (ComponentAp state action m a)
  | Fork (ComponentM state action m Unit) (ReleaseKey -> a)

instance functorComponentF :: Functor m => Functor (ComponentF state action m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe fes k -> Subscribe fes (map f k)
    Release sid a -> Release sid (f a)
    Lift m -> Lift (map f m)
    Par m -> Par (map f m)
    Fork m k -> Fork m (map f k)

newtype ComponentM state action m a
  = ComponentM (Free (ComponentF state action m) a)

hoist :: forall state action m m'. Functor m => (m ~> m') -> ComponentM state action m ~> ComponentM state action m'
hoist nat (ComponentM component) = ComponentM (hoistFree go component)
  where
  go :: ComponentF state action m ~> ComponentF state action m'
  go = case _ of
    State f -> State f
    Subscribe fes k -> Subscribe (\key -> case fes key of EventSource subscribe -> EventSource (nat <<< map nat <<< subscribe)) k
    Release key a -> Release key a
    Lift m -> Lift (nat m)
    Par p -> Par (case p of ComponentAp f -> ComponentAp (hoistFreeAp (hoist nat) f))
    Fork fork k -> Fork (hoist nat fork) k

derive newtype instance functorComponentM :: Functor (ComponentM state action m)

derive newtype instance applyComponentM :: Apply (ComponentM state action m)

derive newtype instance applicativeComponentM :: Applicative (ComponentM state action m)

derive newtype instance bindComponentM :: Bind (ComponentM state action m)

derive newtype instance monadComponentM :: Monad (ComponentM state action m)

instance monadTransComponentM :: MonadTrans (ComponentM state action) where
  lift x = ComponentM (liftF (Lift x))

instance monadEffectComponentM :: MonadEffect m => MonadEffect (ComponentM state action m) where
  liftEffect x = lift (liftEffect x)

instance monadAffComponentM :: MonadAff m => MonadAff (ComponentM state action m) where
  liftAff x = lift (liftAff x)

instance monadStateComponentM :: MonadState state (ComponentM state action m) where
  state x = ComponentM (liftF (State x))

newtype ComponentAp state action m a
  = ComponentAp (FreeAp (ComponentM state action m) a)

derive newtype instance functorComponentAp :: Functor (ComponentAp state action m)

derive newtype instance applyComponentAp :: Apply (ComponentAp state action m)

derive newtype instance applicativeComponentAp :: Applicative (ComponentAp state action m)

data Lifecycle props action
  = Initialize props
  | Props props
  | Action action
  | Finalize

type ComponentInterface state action
  = { stateRef :: Ref state
    , render :: state -> Effect Unit
    , enqueueAction :: action -> Effect Unit
    }

evalComponent :: forall state action. ComponentInterface state action -> ComponentM state action Aff ~> Resource
evalComponent interface (ComponentM component) = runFreeM interpret component
  where
  interpret :: ComponentF state action Aff ~> Resource
  interpret = case _ of
    State f -> do
      liftEffect do
        state <- Ref.read interface.stateRef
        case f state of
          Tuple next state'
            | unsafeRefEq state state' -> do
              Ref.write state' interface.stateRef
              interface.render state'
              pure next
          Tuple next _ -> pure next
    Subscribe prepare next -> do
      canceler <- liftEffect $ Ref.new Nothing
      key <- Resource.register $ liftEffect (Ref.read canceler) >>= sequence_
      runCanceler <- case prepare key of EventSource subscribe -> lift $ subscribe interface.enqueueAction
      liftEffect $ Ref.write (Just runCanceler) canceler
      pure (next key)
    Release key next -> do
      Resource.release key
      pure next
    Lift aff -> do
      liftAff aff
    Par (ComponentAp p) -> do
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalComponent interface) p
    Fork runFork next -> do
      fiber <- Resource.fork $ evalComponent interface runFork
      key <- Resource.register $ Aff.killFiber (Aff.error "Fiber killed") fiber
      pure (next key)

type EvalSpec props action state m
  = { initialize :: props -> Maybe action
    , props :: props -> Maybe action
    , action :: action -> ComponentM state action m Unit
    , finalize :: Maybe action
    }

defaultEval :: forall props action state m. EvalSpec props action state m
defaultEval =
  { initialize: const Nothing
  , props: const Nothing
  , action: const (pure unit)
  , finalize: Nothing
  }

buildEval :: forall props action state m. (EvalSpec props action state m -> EvalSpec props action state m) -> Lifecycle props action -> ComponentM state action m Unit
buildEval f =
  let
    eval = f defaultEval
  in
    case _ of
      Initialize props -> traverse_ eval.action $ eval.initialize props
      Props props -> traverse_ eval.action $ eval.props props
      Action action -> eval.action action
      Finalize -> traverse_ eval.action eval.finalize
