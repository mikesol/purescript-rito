{ name = "purescript-deku"
, dependencies =
  [ "avar"
  , "control"
  , "convertable-options"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
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
  , "sized-vectors"
  , "tuples"
  , "type-equality"
  , "unsafe-coerce"
  , "variant"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
