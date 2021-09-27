{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "data-default"
  , "datetime"
  , "digraph"
  , "effect"
  , "either"
  , "enums"
  , "event"
  , "event-extra"
  , "filterable"
  , "foldable-traversable"
  , "foreign-generic"
  , "foreign-object"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "lists"
  , "math"
  , "maybe"
  , "memoize"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "pqueue"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "refs"
  , "specular"
  , "strings"
  , "taihe"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "uuid"
  , "web-dom"
  , "web-file"
  , "web-html"
  , "web-xhr"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "example/**/*.purs" ]
}
