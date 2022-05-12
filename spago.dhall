{ name = "purescript-deku"
, dependencies =
  [ "avar"
  , "bolson"
  , "control"
  , "convertable-options"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
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
