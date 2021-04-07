{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "axios"
  , "console"
  , "digraph"
  , "effect"
  , "event"
  , "event-extra"
  , "foreign-generic"
  , "generics-rep"
  , "js-timers"
  , "math"
  , "memoize"
  , "newtype"
  , "numbers"
  , "pqueue"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "uuid"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "example/**/*.purs" ]
}
