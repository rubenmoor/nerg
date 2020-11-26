{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "epic-empires"
, dependencies =
  [ "canvas"
  , "colors"
  , "console"
  , "debug"
  , "drawing"
  , "effect"
  , "formatting"
  , "lists"
  , "math"
  , "psci-support"
  , "random"
  , "refs"
  , "signal"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
