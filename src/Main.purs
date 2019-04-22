module Main where

import Component.Root (root)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor (void)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff $ do
    body <- awaitBody
    void $ runUI root unit body
