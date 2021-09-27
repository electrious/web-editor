{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

let overrides = {=}

let additions =
      { event =
        { dependencies = [ "prelude", "filterable" ]
        , repo = "https://github.com/mikesol/purescript-event.git" -- use a forked version that supports purs 0.14
        , version = "932381bb71093acfc4d46ce38513ee759af0588b"
        }
      , event-extra =
        { dependencies = [ "event" ]
        , repo = "https://github.com/manyoo/event-extra.git"
        , version = "v0.1.9"
        }
      , memoize =
        { dependencies = [ "prelude", "strings" ]
        , repo = "https://github.com/paf31/purescript-memoize.git"
        , version = "v5.0.0"
        }
      , digraph =
        { dependencies = [ "newtype", "ordered-collections", "pqueue", "lists" ]
        , repo = "https://github.com/manyoo/purescript-digraph.git"
        , version = "master"
        }
      , pqueue =
        { dependencies = [ "newtype", "ordered-collections", "lists" ]
        , repo = "https://github.com/nullobject/purescript-pqueue.git"
        , version = "v2.0.0"
        }
      , specular =
        { dependencies =
          [ "foreign-object"
          , "aff"
          , "debug"
          , "unsafe-reference"
          , "avar"
          , "typelevel-prelude"
          , "contravariant"
          , "record"
          , "prelude"
          , "random"
          ]
        , repo = "https://github.com/restaumatic/purescript-specular.git"
        , version = "94271b2af04e74961e3bd393e86d80c49f8b9267"
        },
        data-default = {
          dependencies = [ "prelude" ]
        , repo = "https://github.com/manyoo/data-default.git"
        , version = "v0.1.0"
        },
        taihe = {
          dependencies = [ "argonaut-codecs"
                          , "arrays"
                          , "control"
                          , "data-default"
                          , "datetime"
                          , "effect"
                          , "event"
                          , "event-extra"
                          , "filterable"
                          , "foldable-traversable"
                          , "foreign-generic"
                          , "integers"
                          , "lists"
                          , "math"
                          , "maybe"
                          , "newtype"
                          , "numbers"
                          , "ordered-collections"
                          , "prelude"
                          , "profunctor-lenses"
                          , "psci-support"
                          , "transformers"
                          , "tuples"
                          , "unsafe-coerce"
                          , "web-dom"
                          , "web-events"
                          , "web-file"
                          , "web-html"
                          , "web-touchevents"
                          , "web-uievents"
                          ]
        , repo = "https://github.com/manyoo/taihe.git"
        , version = "v0.1.0"
        }
      }

in  upstream // overrides // additions
