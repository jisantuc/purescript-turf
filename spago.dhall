{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "turf"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jisantuc/purescript-turf.git"
}
