{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "epic-empires"
, dependencies =
  [ "arrays"
  , "canvas"
  , "colors"
  , "console"
  , "debug"
  , "drawing"
  , "effect"
  , "foldable-traversable"
  , "formatting"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "refs"
  , "signal"
  , "st"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
