{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "epic-empires"
, dependencies =
  [ "canvas"
  , "colors"
  , "console"
  , "drawing"
  , "effect"
  , "formatting"
  , "psci-support"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
