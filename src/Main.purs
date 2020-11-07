module Main where

import Prelude

import Data.Int (floor, round, toNumber)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Drawing as Drawing
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import Events (gamePeriod, gridBeat, onEvent, onEventE)
import Graphics.Canvas (CanvasElement, canvasElementToImageSource, clearRect, drawImage, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Grid (advance, randomGrid)
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
import Web.HTML.Window (document) as Window

-- [ ] bugfix cgol rules
-- [ ] intelligent scrolling using translate
-- [ ] intelligent zooming using scale
-- [ ] ui canvas on top
-- [ ] live mode: click-activate based cgol
-- [ ] insert custom mode: select flexible size area for custom shape insert
-- [ ] insert library shape mode: select library shape for insertion


main :: Effect Unit
main = do
  w <- window
  document <- Window.document w
  let doc = toDocument document
  mBody <- body document
  let b = unsafePartial fromJust mBody

  canvas <- createElement "canvas" doc
  let canvasElement = unsafeCoerce canvas :: CanvasElement
  ctx <- getContext2D canvasElement
  _ <- appendChild (toNode canvas) (toNode $ toElement b)

  ui <- createElement "canvas" doc
  let uiElement = unsafeCoerce ui :: CanvasElement
  uiCtx <- getContext2D uiElement
  _ <- appendChild (toNode ui) (toNode $ toElement b)

  gridBuffer <- createElement "canvas" doc
  let gridBufferElement = unsafeCoerce gridBuffer :: CanvasElement
  gridBufferCtx <- getContext2D gridBufferElement

  scrollBuffer <- createElement "canvas" doc
  let scrollBufferElement = unsafeCoerce scrollBuffer :: CanvasElement
  scrollBufferCtx <- getContext2D scrollBufferElement

  -- | globals

  -- grid state
  refGridState <- join $ new <$> randomGrid 0.1

  -- buffer ready for redraw
  refGridBufferReady <- new false
  refScrollBufferReady <- new false

  -- frame rate
  refFrameCount <- new 0
  refFrameRate <- new 0

  -- zoomFactor: number of pixels per cell
  refZoomFactor <- new 5
  -- zoomFactor 5: 2 to 3 frames while scrolling
  onEventE (Signal.wheelY canvas) $ \deltaY -> do
    z <- read refZoomFactor
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    -- when (deltaY > 0.0
    --    || Grid.width * z > round width
    --    || Grid.height * z > round height) $
    --   modify_ (\x -> max 1 $ x + round deltaY) refZoomFactor
    let delta | deltaY > 0.0 = 1
              | deltaY < 0.0 && z > 3 = -1
              | otherwise = 0
    write (z + delta) refZoomFactor

  -- cell coordinates of canvas center
  refViewPos <- new $ Tuple 0.0 0.0
  refMousePos <- new $ Tuple 0 0
  refMouseVPos <- new $ Tuple 0.0 0.0
  refGridPos <- new $ Tuple 0 0

  sMouse1 <- Signal.mouseButtonPressed Signal.MouseLeftButton
  sMousePos <- Signal.mousePos

  onEvent sMouse1 $ \t -> when t $ do
    {x: x, y: y} <- Signal.get sMousePos
    write (Tuple x y) refMousePos

  let redrawUI = do
        frameRate <- read refFrameRate
        zoomFactor <- read refZoomFactor
        Tuple mouseX mouseY <- read refMousePos
        Tuple mouseVX mouseVY <- read refMouseVPos
        Tuple gridX gridY <- read refGridPos
        width <- getCanvasWidth canvasElement
        height <- getCanvasHeight canvasElement
        Tuple viewX viewY <- read refViewPos
        clearRect uiCtx { x: 0.0, y: 0.0, width, height }
        render uiCtx $ Drawing.redrawUI frameRate
                                        zoomFactor
                                        mouseX mouseY
                                        mouseVX mouseVY
                                        gridX gridY
                                        width height
                                        viewX viewY

  -- TODO: redrawFull

  let redrawFull = do
        cellStates <- _.cellStates <$> read refGridState
        width <- getCanvasWidth canvasElement
        height <- getCanvasHeight canvasElement
        Tuple viewX viewY <- read refViewPos
        zoomFactor <- read refZoomFactor

        clearRect gridBufferCtx { x: 0.0, y: 0.0, width, height }
        render gridBufferCtx $ Drawing.redrawFull cellStates
                                                  width height
                                                  viewX viewY
                                                  zoomFactor
        write true refGridBufferReady

  let redrawDelta = do
        changes <- _.changes <$> read refGridState
        width <- getCanvasWidth canvasElement
        height <- getCanvasHeight canvasElement
        Tuple viewX viewY <- read refViewPos
        zoomFactor <- read refZoomFactor
        render gridBufferCtx $ Drawing.redrawDelta changes
                                                   width height
                                                   viewX viewY
                                                   zoomFactor
        write true refGridBufferReady

  onEventE Signal.animationFrame $ \_ -> do
    -- check if resize is necessary
    width <- clientWidth canvas
    height <- clientHeight canvas
    canvasWidth <- getCanvasWidth canvasElement
    canvasHeight <- getCanvasHeight canvasElement
    unless (width == canvasWidth) $ do setCanvasWidth canvasElement width
                                       setCanvasWidth scrollBufferElement width
                                       setCanvasWidth gridBufferElement width
                                       setCanvasWidth uiElement width
    unless (height == canvasHeight) $ do setCanvasHeight canvasElement height
                                         setCanvasHeight scrollBufferElement height
                                         setCanvasHeight gridBufferElement height
                                         setCanvasHeight uiElement height
    modify_ (\x -> x + 1) refFrameCount

    redrawUI

    gridBufferReady <- read refGridBufferReady
    if gridBufferReady
    then (do
      clearRect ctx { x: 0.0, y: 0.0, width, height }
      drawImage ctx (canvasElementToImageSource gridBufferElement) 0.0 0.0
      write false refGridBufferReady
      )
    else whenM (read refScrollBufferReady) $ do
      clearRect ctx { x: 0.0, y: 0.0, width, height}
      drawImage ctx (canvasElementToImageSource scrollBufferElement) 0.0 0.0
      write false refScrollBufferReady

  onEvent gridBeat $ \_ -> do
    c <- read refFrameCount
    write (round(1000.0 * toNumber c / gamePeriod)) refFrameRate
    write 0 refFrameCount

    { changes: _, cellStates, neighbors } <- read refGridState
    write (advance cellStates neighbors) refGridState

    redrawDelta

  onEvent sMousePos $ \{x: x, y: y} -> do
    -- grid position label
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    z <- toNumber <$> read refZoomFactor
    Tuple viewX viewY <- read refViewPos
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
          viewXMoved = viewX - toNumber deltaX / z
          halfWidth = (if viewXMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.width / 2)
          viewXNew = (viewXMoved + halfWidth) % toNumber Grid.width - halfWidth
          viewYMoved = viewY + toNumber deltaY / z
          halfHeight = (if viewYMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.height / 2)
          viewYNew = (viewYMoved + halfHeight) % toNumber Grid.height - halfHeight

      write (Tuple viewXNew viewYNew) refViewPos
      clearRect scrollBufferCtx { x: 0.0, y: 0.0, width, height}
      drawImage scrollBufferCtx (canvasElementToImageSource canvasElement)
                                (toNumber deltaX) (toNumber deltaY)
      write true refScrollBufferReady

    write (Tuple x y) refMousePos

  redrawFull
