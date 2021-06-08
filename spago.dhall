{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "arrays"
, dependencies =
  [ "assert"
  , "console"
  , "debug"
  , "erl-atom"
  , "erl-lists"
  , "erl-tuples"
  , "foreign"
  , "free"
  , "prelude"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
