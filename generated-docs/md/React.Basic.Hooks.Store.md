## Module React.Basic.Hooks.Store

#### `Instance`

``` purescript
type Instance props state m = { props :: props, readProps :: m props, readState :: m state, setState :: (state -> state) -> m Unit, state :: state }
```

A stores internal interface to itself, only accessible inside the `update` function.

#### `Spec`

``` purescript
type Spec props state action m = { init :: state, launch :: m Unit -> Aff Unit, props :: props, update :: Instance props state m -> action -> m Unit }
```

The spec required to configure and run a store.

#### `Store`

``` purescript
type Store state action = { dispatch :: action -> Effect Unit, readState :: Effect state, state :: state }
```

A stores external interface, returned from `useStore`.

#### `UseStore`

``` purescript
newtype UseStore props state action hooks
```

##### Instances
``` purescript
Newtype (UseStore props state action hooks) _
```

#### `useStore`

``` purescript
useStore :: forall m props state action. MonadEffect m => Eq props => Spec props state action m -> Hook (UseStore props state action) (Store state action)
```


