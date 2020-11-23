module Main where

import Prelude

import Data.Int (floor, round, toNumber)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, snd)
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

  buffer <- createElement "canvas" doc
  let bufferElement = unsafeCoerce buffer :: CanvasElement
  bufferCtx <- getContext2D bufferElement

  -- | globals

  -- grid state
  refGridState <- join $ new <$> randomGrid 0.1
  refChanges <- new ([] :: Grid.Changes)

  refScrollDelta <- new $ Tuple 0 0

  -- frame rate
  refFrameCount <- new 0
  refFrameRate <- new 0

  -- zoomFactor: number of pixels per cell
  refZoomFactor <- new 50
  -- zoomFactor 5: 2 to 3 frames while scrolling

  onEventE (Signal.wheelY canvas) $ \deltaY -> do
    -- TODO: like scrolling, accumulate deltaY here and apply zoom in animation frame
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
        viewPos <- read refViewPos
        clearRect uiCtx { x: 0.0, y: 0.0, width, height }
        render uiCtx $ Drawing.redrawUI frameRate
                                        zoomFactor
                                        mouseX mouseY
                                        mouseVX mouseVY
                                        gridX gridY
                                        width height
                                        viewPos

  -- TODO: allow to specify range for full redraw
  let redrawFull canvasCtx = do
        cellStates <- _.cellStates <$> read refGridState
        width <- getCanvasWidth canvasElement
        height <- getCanvasHeight canvasElement
        viewPos <- read refViewPos
        zoomFactor <- read refZoomFactor

        clearRect canvasCtx { x: 0.0, y: 0.0, width, height }
        render canvasCtx $ Drawing.redrawFull cellStates
                                              width height
                                              viewPos
                                              zoomFactor

  onEventE Signal.animationFrame $ \_ -> do
    -- check if resize is necessary
    width <- clientWidth canvas
    height <- clientHeight canvas
    canvasWidth <- getCanvasWidth canvasElement
    canvasHeight <- getCanvasHeight canvasElement
    unless (width == canvasWidth) $ do setCanvasWidth canvasElement width
                                       setCanvasWidth bufferElement width
                                       setCanvasWidth uiElement width
    unless (height == canvasHeight) $ do setCanvasHeight canvasElement height
                                         setCanvasHeight bufferElement height
                                         setCanvasHeight uiElement height
    modify_ (add 1) refFrameCount

    -- get changes of cells' states
    changes <- read refChanges

    zoomFactor <- read refZoomFactor
    viewPos <- read refViewPos

    -- copy current canvas to buffer
    clearRect bufferCtx { x: 0.0, y: 0.0, width, height }
    drawImage bufferCtx (canvasElementToImageSource canvasElement) 0.0 0.0

    -- redraw changes on buffer
    -- TODO: make redrawDelta aware of current view
    render bufferCtx $ Drawing.redrawDelta changes
                                           width height
                                           viewPos
                                           zoomFactor
    -- clear changes cache
    write [] refChanges

    -- TODO: apply zoom scaling to buffer
    -- copy buffer image to canvas AND apply scroll translation to buffer
    delta <- read refScrollDelta
    unless (delta == Tuple 0 0) $ do
      modify_ (moveView (toNumber zoomFactor) delta) refViewPos
      write (Tuple 0 0) refScrollDelta
    clearRect ctx { x: 0.0, y: 0.0, width, height}
    drawImage ctx (canvasElementToImageSource bufferElement)
                  (toNumber $ fst delta) (toNumber $ snd delta)

    -- TODO: full redraw on gaps caused by scrolling/zooming

    redrawUI

  onEvent gridBeat $ \_ -> do
    c <- read refFrameCount
    write (round $ 1000.0 * toNumber c / gamePeriod) refFrameRate
    write 0 refFrameCount

    gridState <- read refGridState
    let Tuple changes newGridState = advance gridState
    write newGridState refGridState
    write changes refChanges

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
      flip modify_ refScrollDelta $ \(Tuple dx dy) ->
        Tuple (dx + x - oldX) (dy + y - oldY)

    write (Tuple x y) refMousePos

  redrawFull ctx

moveView :: Number -> Tuple Int Int -> Tuple Number Number -> Tuple Number Number
moveView z (Tuple deltaX deltaY) (Tuple vx vy) =
  let viewXMoved = vx - toNumber deltaX / z
      halfWidth = (if viewXMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.width / 2)
      viewXNew = (viewXMoved + halfWidth) % toNumber Grid.width - halfWidth

      viewYMoved = vy + toNumber deltaY / z
      halfHeight = (if viewYMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.height / 2)
      viewYNew = (viewYMoved + halfHeight) % toNumber Grid.height - halfHeight
  in  Tuple viewXNew viewYNew
