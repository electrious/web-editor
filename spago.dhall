{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "axios"
  , "console"
  , "easy-ffi"
  , "effect"
  , "event"
  , "event-extra"
  , "foreign-generic"
  , "generics-rep"
  , "js-timers"
  , "math"
  , "memoize"
  , "newtype"
  , "profunctor-lenses"
  , "psci-support"
  , "uuid"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "example/**/*.purs" ]
}
