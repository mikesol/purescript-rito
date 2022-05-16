{ name = "purescript-deku"
, dependencies =
  [ "aff"
  , "avar"
  , "bolson"
  , "control"
  , "convertable-options"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "exists"
  , "fast-vect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "random"
  , "record"
  , "refs"
  , "safe-coerce"
  , "unsafe-coerce"
  , "variant"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
