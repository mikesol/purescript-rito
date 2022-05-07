{ name = "purescript-deku"
, dependencies =
  [ "avar"
  , "control"
  , "convertable-options"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
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
  , "unsafe-coerce"
  , "variant"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
