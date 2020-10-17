module React.Store.Internal where

import Prelude
import Control.Applicative.Free (FreeAp, hoistFreeAp, retractFreeAp)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Resource (ReleaseKey, Resource, ResourceT(..))
import Control.Monad.Resource as Resource
import Control.Monad.Resource.Registry (Registry)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (lmap)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype ForkId
  = ForkId ReleaseKey

newtype SubscriptionId
  = SubscriptionId ReleaseKey

newtype EventSource a
  = EventSource ((a -> Aff Unit) -> Aff (Aff Unit))

data StoreF state action m a
  = State (state -> Tuple a state)
  | Subscribe (SubscriptionId -> EventSource action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | Par (ComponentAp state action m a)
  | Fork (ComponentM state action m Unit) (ForkId -> a)
  | Kill ForkId a

instance functorStoreF :: Functor m => Functor (StoreF state action m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe fes k -> Subscribe fes (map f k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Lift m -> Lift (map f m)
    Par m -> Par (map f m)
    Fork m k -> Fork m (map f k)
    Kill fid a -> Kill fid (f a)

newtype ComponentM state action m a
  = ComponentM (Free (StoreF state action m) a)

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
  | Update props
  | Action action
  | Finalize

interpret :: forall state action a. Ref state -> (action -> Aff Unit) -> ComponentM state action Aff a -> Resource a
interpret stateRef enqueueAction (ComponentM store) =
  runFreeM
    ( case _ of
        State f -> do
          liftEffect do
            state <- Ref.read stateRef
            case f state of
              Tuple next state' -> do
                Ref.write state' stateRef
                pure next
        Subscribe prepare next -> do
          canceler <- liftEffect $ Ref.new Nothing
          key <- Resource.register $ liftEffect (Ref.read canceler) >>= sequence_
          let
            subscriptionId = SubscriptionId key

            EventSource subscriber = prepare subscriptionId
          runCanceler <- lift $ subscriber enqueueAction
          liftEffect $ Ref.write (Just runCanceler) canceler
          pure (next subscriptionId)
        Unsubscribe (SubscriptionId key) next -> do
          Resource.release key
          pure next
        Lift aff -> do
          lift aff
        Par (ComponentAp p) -> do
          fromReaderT $ sequential $ retractFreeAp $ hoistFreeAp (parallel <<< toReaderT <<< interpret stateRef enqueueAction) p
        Fork run next -> do
          fiber <- Resource.fork $ interpret stateRef enqueueAction run
          key <- Resource.register $ Aff.killFiber (Aff.error "Fiber killed") fiber
          pure $ next (ForkId key)
        Kill (ForkId key) next -> do
          Resource.release key
          pure next
    )
    store
  where
  toReaderT :: forall m. ResourceT m ~> ReaderT Registry m
  toReaderT (ResourceT run) = ReaderT run

  fromReaderT :: forall m. ReaderT Registry m ~> ResourceT m
  fromReaderT (ReaderT run) = ResourceT run
