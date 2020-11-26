module Main where

import Prelude

import Data.Array (length)
import Data.Int (floor, round, toNumber)
import Data.Maybe (fromJust)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (trace)
import Drawing as Drawing
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import Events (gamePeriod, gridBeat, onEvent, onEventE)
import Graphics.Canvas (CanvasElement, Context2D, canvasElementToImageSource, clearRect, drawImage, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Grid (GridState, advance, randomGrid)
import Grid as Grid
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Signal (get) as Signal
import Signal.DOM (MouseButton(..), mouseButtonPressed, mousePos, animationFrame, wheelY) as Signal
import Signal.DOM (keyPressed)
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
-- [x] ui canvas on top
-- [ ] live mode: click-activate based cgol
-- [ ] insert custom mode: select flexible size area for custom shape insert
-- [ ] insert library shape mode: select library shape for insertion

keyCodeSpacebar :: Int
keyCodeSpacebar = 32

-- | globals
type AppState =
  {
    -- grid state
    gridState :: GridState
  , changes :: Grid.Changes

  , scrollDelta :: Tuple Int Int

  -- frame rate
  , frameCount :: Int
  , frameRate :: Int

  -- zoomFactor: number of pixels per cell
  , zoomFactor :: Int
  -- zoomFactor 5: 2 to 3 frames while scrolling
  -- cell coordinates of canvas center
  , viewPos :: Tuple Number Number

  , mousePos :: Tuple Int Int
  , mouseVPos :: Tuple Number Number
  , gridPos :: Tuple Int Int
  }

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

  -- initialize app state
  initialGrid <- randomGrid 0.1
  let initialAppState =
        { gridState: initialGrid
        , changes: []

        , scrollDelta: Tuple 0 0

        -- frame rate
        , frameCount: 0
        , frameRate: 0

        -- zoomFactor: number of pixels per cell
        , zoomFactor: 50
        -- zoomFactor 5: 2 to 3 frames while scrolling
        -- cell coordinates of canvas center
        , viewPos: Tuple 0.0 0.0

        , mousePos: Tuple 0 0
        , mouseVPos: Tuple 0.0 0.0
        , gridPos: Tuple 0 0
        }
  refAppState <- new initialAppState

  onEventE (Signal.wheelY canvas) $ \deltaY -> do
    -- TODO: like scrolling, accumulate deltaY here and apply zoom in animation frame
    app <- read refAppState
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    -- when (deltaY > 0.0
    --    || Grid.width * z > round width
    --    || Grid.height * z > round height) $
    --   modify_ (\x -> max 1 $ x + round deltaY) refZoomFactor
    let delta | deltaY > 0.0 = 1
              | deltaY < 0.0 && app.zoomFactor > 3 = -1
              | otherwise = 0
    write (app { zoomFactor = app.zoomFactor + delta }) refAppState

  sMouse1 <- Signal.mouseButtonPressed Signal.MouseLeftButton
  sMousePos <- Signal.mousePos

  onEvent sMouse1 $ \t -> when t $ do
    {x, y} <- Signal.get sMousePos
    modify_ (_ { mousePos = Tuple x y }) refAppState

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
    -- copy current canvas to buffer
    clearRect bufferCtx { x: 0.0, y: 0.0, width, height }
    drawImage bufferCtx (canvasElementToImageSource canvasElement) 0.0 0.0

    app <- read refAppState
    -- redraw changes on buffer
    -- TODO: make redrawDelta aware of current view
    render bufferCtx $ Drawing.redrawDelta app.changes
                                           width height
                                           app.viewPos
                                           app.zoomFactor
    -- clear changes cache
    flip modify_ refAppState $ \a ->
      a { frameCount = a.frameCount + 1
        , changes = []
        }

    clearRect ctx { x: 0.0, y: 0.0, width, height}
    drawImage ctx (canvasElementToImageSource bufferElement)
                  (toNumber $ fst app.scrollDelta) (toNumber $ snd app.scrollDelta)

    -- TODO: full redraw on gaps caused by scrolling/zooming

    -- TODO: apply zoom scaling to buffer
    -- copy buffer image to canvas AND apply scroll translation to buffer
    unless (app.scrollDelta == Tuple 0 0) $
      flip modify_ refAppState \a ->
        a { viewPos = moveView (toNumber a.zoomFactor) a.scrollDelta a.viewPos
          , scrollDelta = Tuple 0 0
          }

    app' <- read refAppState
    redrawUI app' width height uiCtx

  -- onEvent gridBeat $ \_ -> do
  spaceBarDown <- keyPressed keyCodeSpacebar
  onEvent spaceBarDown \downUp -> when downUp $ do
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    flip modify_ refAppState \app ->
      let widthC = width / toNumber app.zoomFactor
          heightC = height / toNumber app.zoomFactor
          Tuple changes gridState = advance app.gridState widthC heightC app.viewPos
      in  trace (show $ Tuple changes (length changes)) \_ -> app { changes = changes
              , gridState = gridState
              , frameRate = round $ 1000.0 * toNumber app.frameCount / gamePeriod
              , frameCount = 0
              }

  onEvent sMousePos $ \{x, y} -> do
    app <- read refAppState
    -- grid position label
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    let Tuple viewX viewY = app.viewPos
        z = toNumber app.zoomFactor
        gridX = floor $ (toNumber x - width / 2.0) / z + viewX
        gridY = floor $ (height / 2.0 - toNumber y) / z + viewY
        mouseVX = toNumber (floor $ ((toNumber x - width / 2.0) / z + viewX) * 10.0) / 10.0
        mouseVY = toNumber (floor $ ((height / 2.0 - toNumber y) / z + viewY) * 10.0) / 10.0

    -- drag mouse to scroll
    whenM (Signal.get sMouse1) $ do
      let Tuple oldX oldY = app.mousePos
          Tuple dx dy = app.scrollDelta
      flip modify_ refAppState $
        _ { scrollDelta = Tuple (dx + x - oldX) (dy + y - oldY) }

    flip modify_ refAppState $
      _ { gridPos = Tuple gridX gridY
        , mouseVPos = Tuple mouseVX mouseVY
        , mousePos = Tuple x y
        }

  width <- getCanvasWidth canvasElement
  height <- getCanvasHeight canvasElement
  redrawFull initialAppState width height ctx

redrawUI :: AppState -> Number -> Number -> Context2D -> Effect Unit
redrawUI app width height uiCtx = do
  clearRect uiCtx { x: 0.0, y: 0.0, width, height }
  render uiCtx $ Drawing.redrawUI app.frameRate
                                  app.zoomFactor
                                  app.mousePos
                                  app.mouseVPos
                                  app.gridPos
                                  width height
                                  app.viewPos

-- TODO: allow to specify range for full redraw
redrawFull :: AppState -> Number -> Number -> Context2D -> Effect Unit
redrawFull app width height canvasCtx = do
  clearRect canvasCtx { x: 0.0, y: 0.0, width, height }
  let widthC = width / toNumber app.zoomFactor
      heightC = height / toNumber app.zoomFactor
      Tuple viewX viewY = app.viewPos
      left = viewX - widthC / 2.0
      bottom = viewY - heightC / 2.0
  render canvasCtx $ Drawing.redrawFull' app.gridState.cellStates
                                        width height
                                        left bottom
                                        widthC heightC
                                        app.viewPos
                                        app.zoomFactor

moveView :: Number -> Tuple Int Int -> Tuple Number Number -> Tuple Number Number
moveView z (Tuple deltaX deltaY) (Tuple vx vy) =
  let viewXMoved = vx - toNumber deltaX / z
      halfWidth = (if viewXMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.width / 2)
      viewXNew = (viewXMoved + halfWidth) % toNumber Grid.width - halfWidth

      viewYMoved = vy + toNumber deltaY / z
      halfHeight = (if viewYMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.height / 2)
      viewYNew = (viewYMoved + halfHeight) % toNumber Grid.height - halfHeight
  in  Tuple viewXNew viewYNew
