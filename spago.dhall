{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "behaviors"
  , "console"
  , "easy-ffi"
  , "effect"
  , "event"
  , "generics-rep"
  , "math"
  , "psci-support"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
