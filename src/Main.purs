module Main where

import Prelude

import Data.Int (round, toNumber)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Drawing as Drawing
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import Events (gamePeriod, gridBeat, onEvent, onEventE)
import Graphics.Canvas (getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Partial.Unsafe (unsafePartial)
import Signal (get) as Signal
import Signal.DOM (MouseButton(..), mouseButtonPressed, mousePos, animationFrame, wheelY) as Signal
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (createElement)
import Web.DOM.Element (clientHeight, clientWidth, toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  w <- window
  doc <- document w
  mBody <- body doc
  let b = unsafePartial fromJust mBody
  el <- createElement "canvas" $ toDocument doc
  _ <- appendChild (toNode el) (toNode $ toElement b)

  -- | globals
  refFrameCount <- new 0
  refFrameRate <- new 0
  let pollFrameRate = gridBeat ~> \_ -> do
        c <- read refFrameCount
        write (c `div` round gamePeriod) refFrameRate
        write 0 refFrameCount

  refViewX <- new 0.4
  refViewY <- new 0.5
  refZoomFactor <- new 100

  runSignal pollFrameRate

  let canvas = unsafeCoerce el
  displayWidth <- clientWidth el
  displayHeight <- clientHeight el

  -- TODO: move to some resize event, and swap displayWidth/Height with canvasWidth/Height
  -- resize if display size has changed
  canvasWidth <- getCanvasWidth canvas
  canvasHeight <- getCanvasHeight canvas
  unless (canvasWidth == displayWidth) $ setCanvasWidth canvas displayWidth
  unless (canvasHeight == displayHeight) $ setCanvasHeight canvas displayHeight
  ctx <- getContext2D canvas

  let redraw = do
        frameRate <- read refFrameRate
        viewX <- read refViewX
        viewY <- read refViewY
        zoomFactor <- read refZoomFactor
        let params :: Drawing.Params
            params =
              { frameRate: frameRate
              , width: round canvasWidth
              , height: round canvasHeight
              , viewX: viewX
              , viewY: viewY
              , zoomFactor: zoomFactor
              }
        render ctx $ Drawing.redraw params

  -- redraw loop based on RAF, not meant to do anything else but to redraw the
  -- canvas
  let loop = do
        redraw
        _ <- requestAnimationFrame loop w
        modify_ (\x -> x + 1) refFrameCount
        pure unit
  loop

gridBeat :: Signal Number
gridBeat = every gamePeriod
