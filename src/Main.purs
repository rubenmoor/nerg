module Main where

import Prelude

import Canvas as Canvas
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document, requestAnimationFrame)

main :: Effect Unit
main = do
  w <- window
  doc <- document w
  mBody <- body doc
  let b = unsafePartial fromJust mBody
  el <- createElement "canvas" $ toDocument doc
  _ <- appendChild (toNode el) (toNode $ toElement b)

  let loop = do
        Canvas.redraw el
        _ <- requestAnimationFrame loop w
        pure unit
  loop
