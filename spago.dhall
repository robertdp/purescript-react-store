{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "avar"
  , "console"
  , "exists"
  , "free"
  , "freeap"
  , "prelude"
  , "react-basic-hooks"
  , "resourcet"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
