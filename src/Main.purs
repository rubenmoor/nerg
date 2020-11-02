module Main where

import Prelude

import Data.Int (floor, round, toNumber)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Drawing as Drawing
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import Events (gamePeriod, gridBeat, onEvent, onEventE)
import Graphics.Canvas (CanvasElement, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Grid (randomGrid)
import Grid as Grid
import Math ((%))
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

-- [x] hover highlight current cell
-- [x] grid with fixed dimensions and maximum zoom
-- [x] toroidal render
-- [ ] live mode: click-activate based cgol
-- [ ] random start distribution
-- [ ] insert custom mode: select flexible size area for custom shape insert
-- [ ] insert library shape mode: select library shape for insertion


main :: Effect Unit
main = do
  w <- window
  doc <- document w
  mBody <- body doc
  let b = unsafePartial fromJust mBody
  canvas <- createElement "canvas" $ toDocument doc
  _ <- appendChild (toNode canvas) (toNode $ toElement b)
  let canvasElement = unsafeCoerce canvas :: CanvasElement
  ctx <- getContext2D canvasElement

  -- | globals

  -- grid state
  refGrid <- join $ new <$> randomGrid 0.1

  -- frame rate
  refFrameCount <- new 0
  refFrameRate <- new 0
  onEvent gridBeat $ \_ -> do
    c <- read refFrameCount
    write (round(1000.0 * toNumber c / gamePeriod)) refFrameRate
    write 0 refFrameCount

  -- zoomFactor: number of pixels per cell

  -- JS Array: 44/45 fps, crash when decreasing zoom
  -- Lazy list/lists: 4/5 fps, no crash
  refZoomFactor <- new 7
  onEventE (Signal.wheelY canvas) $ \deltaY -> do
    z <- read refZoomFactor
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    -- when (deltaY > 0.0
    --    || Grid.width * z > round width
    --    || Grid.height * z > round height) $
    --   modify_ (\x -> max 1 $ x + round deltaY) refZoomFactor
    modify_ (\x -> max 1 $ x + round deltaY) refZoomFactor

  -- cell coordinates of canvas center
  refViewX <- new 0.5
  refViewY <- new 0.5
  refMousePos <- new $ Tuple 0 0
  refMouseVPos <- new $ Tuple 0.0 0.0
  refGridPos <- new $ Tuple 0 0

  sMouse1 <- Signal.mouseButtonPressed Signal.MouseLeftButton
  sMousePos <- Signal.mousePos

  onEvent sMouse1 $ \t -> when t $ do
    {x: x, y: y} <- Signal.get sMousePos
    write (Tuple x y) refMousePos

  onEvent sMousePos $ \{x: x, y: y} -> do
    -- grid position label
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    z <- toNumber <$> read refZoomFactor
    viewX <- read refViewX
    viewY <- read refViewY
    let gridX = floor $ (toNumber x - width / 2.0) / z + viewX
        gridY = floor $ (height / 2.0 - toNumber y) / z + viewY
        mouseVX = toNumber (floor $ ((toNumber x - width / 2.0) / z + viewX) * 10.0) / 10.0
        mouseVY = toNumber (floor $ ((height / 2.0 - toNumber y) / z + viewY) * 10.0) / 10.0
    write (Tuple gridX gridY) refGridPos
    write (Tuple mouseVX mouseVY) refMouseVPos

    -- drag mouse to scroll
    leftButtonDown <- Signal.get sMouse1
    when leftButtonDown $ do
      Tuple oldX oldY <- read refMousePos
      let deltaX = x - oldX
          deltaY = y - oldY
          -- viewX == Grid.width / 2 <=> viewX == -Grid.width / 2
      flip modify_ refViewX $ \x' ->
        let newX = x' - toNumber deltaX / z
            halfWidth = toNumber $ Grid.width / 2
        in  (newX + halfWidth % toNumber Grid.width) - halfWidth
      flip modify_ refViewY $ \y' ->
        let newY = y' + toNumber deltaY / z
            halfHeight = toNumber $ Grid.height / 2
        in  (newY + halfHeight % toNumber Grid.height) - halfHeight

    write (Tuple x y) refMousePos

  let redraw = do
        Tuple gridState _ <- read refGrid
        -- check if resize is necessary
        width <- clientWidth canvas
        height <- clientHeight canvas
        canvasWidth <- getCanvasWidth canvasElement
        canvasHeight <- getCanvasHeight canvasElement
        unless (width == canvasWidth) $ setCanvasWidth canvasElement width
        unless (height == canvasHeight) $ setCanvasHeight canvasElement height

        frameRate <- read refFrameRate
        viewX <- read refViewX
        viewY <- read refViewY
        zoomFactor <- read refZoomFactor
        mousePos <- read refMousePos
        mouseVPos <- read refMouseVPos
        gridPos <- read refGridPos

        let params :: Drawing.Params
            params =
              { gridState: gridState
              , frameRate: frameRate
              , canvasDims: Tuple (round width) $ round height
              , viewPos: Tuple viewX viewY
              , zoomFactor: zoomFactor
              , mousePos: mousePos
              , mouseVPos: mouseVPos
              , gridPos: gridPos
              }
        render ctx $ Drawing.redraw params

  onEventE Signal.animationFrame $ \_ -> do
    redraw
    modify_ (\x -> x + 1) refFrameCount
