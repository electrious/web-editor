{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "easy-ffi"
  , "effect"
  , "event"
  , "generics-rep"
  , "js-timers"
  , "math"
  , "psci-support"
  , "uuid"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
