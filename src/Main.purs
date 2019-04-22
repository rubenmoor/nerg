module Main where

import Prelude

import Component.Root (root)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff $ do
    body <- awaitBody
    runUI root unit body
