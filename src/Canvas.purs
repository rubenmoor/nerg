module Canvas
 ( redraw
 ) where

import Prelude

import Data.Int (round)
import Drawing as Drawing
import Effect (Effect)
import Graphics.Canvas (getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (clientHeight, clientWidth)

redraw :: Element -> Effect Unit
redraw el =
  do
    let canvas = unsafeCoerce el
    displayWidth <- clientWidth el
    displayHeight <- clientHeight el

    -- resize if display size has changed
    canvasWidth <- getCanvasWidth canvas
    canvasHeight <- getCanvasHeight canvas
    unless (canvasWidth == displayWidth) $ setCanvasWidth canvas displayWidth
    unless (canvasHeight == displayHeight) $ setCanvasHeight canvas displayHeight

    let params :: Drawing.Params
        params =
          { width: round displayWidth
          , height: round displayHeight
          , viewX: 0.4
          , viewY: 0.5
          , zoomFactor: 100
          }
    ctx <- getContext2D canvas
    render ctx $ Drawing.redraw params
