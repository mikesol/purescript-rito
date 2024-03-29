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
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
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
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220624/packages.dhall
        sha256:08989ed9f53e381f879f1b7012ad7684b1ed64d7164c4ad75e306d3210a46c92

let overrides =
      { bolson =
        { dependencies = [ "prelude", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "main"
        }
      , deku =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "main"
        }
      , ocarina =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arraybuffer-types"
          , "avar"
          , "bolson"
          , "control"
          , "convertable-options"
          , "effect"
          , "either"
          , "exceptions"
          , "fast-vect"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "homogeneous"
          , "hyrule"
          , "indexed-monad"
          , "integers"
          , "js-timers"
          , "lists"
          , "maybe"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "random"
          , "refs"
          , "safe-coerce"
          , "simple-json"
          , "sized-vectors"
          , "tuples"
          , "type-equality"
          , "typelevel"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "unsafe-reference"
          , "variant"
          , "web-events"
          , "web-file"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-ocarina.git"
        , version = "main"
        }
      , hyrule =
        { dependencies =
          [ "monoid-extras", "web-uievents", "js-timers", "unsafe-reference" ]
        , repo = "https://github.com/mikesol/purescript-hyrule.git"
        , version = "master"
        }
      }

in  upstream // overrides
