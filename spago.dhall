{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "turf"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "assert"
  , "console"
  , "effect"
  , "foreign-object"
  , "psci-support"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jisantuc/purescript-turf.git"
}
