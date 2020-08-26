# purescript-react-store

Asynchronous state management made easy(er).

This library provides the hook `useStore` for `react-basic-hooks`. It offers a straightforward and convenient approach to asynchronous state management for components.

```purs
useStore ::
  forall m props state action.
  MonadEffect m =>
  { props :: props
  , init :: state
  , update ::
      { props :: props
      , state :: state
      , setState :: (state -> state) -> m Unit
      , readProps :: m props
      , readState :: m state
      } ->
      action ->
      m Unit
  , launch :: m Unit -> Aff Unit
  } ->
  Hook (UseStore props state action m)
    { state :: state
    , dispatch :: action -> Effect Unit
    , readState :: Effect state
    }
```

If you're not sure you need this library, try the hooks exposed by `react-basic-hooks` first. Just using `useEffect` and `useReducer` can get you a long way, and that library also includes `useAffReducer` for an Elm-ish approach.
