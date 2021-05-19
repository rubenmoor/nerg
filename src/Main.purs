module Main where

import Prelude

import Data.Array (find, length, (!!), null)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), snd)
import Drawing as Drawing
import Effect (Effect)
import Effect.Ref (modify_, new, read, write)
import Events (gamePeriod, gridBeat, onEvent, onEventE)
import Graphics.Canvas (CanvasElement, Context2D, canvasElementToImageSource, clearRect, drawImage, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Grid (Change, GridState, advance, indexToViewCoords, randomGrid, emptyGrid)
import Grid as Grid
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Signal (get) as Signal
import Signal.DOM (MouseButton(..), mouseButtonPressed, mousePos, animationFrame, wheelY) as Signal
import Signal.DOM (keyPressed)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (createElement, documentURI)
import Web.DOM.Element (clientHeight, clientWidth, toNode, setAttribute, toEventTarget)
import Web.DOM.Node (appendChild)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.Event.Event (EventType (..), stopPropagation, preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document) as Window

-- [x] bugfix cgol rules
-- [x] intelligent scrolling using translate
-- [ ] intelligent zooming using scale
-- [x] ui canvas on top
-- [ ] live mode: click-activate based cgol
-- [ ] insert custom mode: select flexible size area for custom shape insert
-- [ ] insert library shape mode: select library shape for insertion

keyCodeSpacebar :: Int
keyCodeSpacebar = 32

keyCodeR :: Int
keyCodeR = 82

-- | globals
type AppState =
  {
    -- grid state
    gridState :: GridState
  , changes :: Grid.Changes

  , scrollDelta :: Maybe (Tuple Int Int)

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
  , gridPosFromIndex :: Tuple Int Int
  , gridIndex :: Int
  , currentState :: String
  , currentChange :: Maybe Change
  , currentNNeighbors :: String
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

  noContextMenu <- eventListener $ \e -> do
    preventDefault e
    stopPropagation e
  addEventListener (EventType "contextmenu") noContextMenu true (toEventTarget ui)

  buffer <- createElement "canvas" doc
  let bufferElement = unsafeCoerce buffer :: CanvasElement
  bufferCtx <- getContext2D bufferElement

  -- initialize app state
  -- initialGrid <- randomGrid 0.1
  let (initialAppState :: AppState) =
        { gridState: emptyGrid -- initialGrid
        , changes: []

        , scrollDelta: Nothing

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
        , gridPosFromIndex: Tuple 0 0
        , gridIndex: 0
        , currentState: ""
        , currentChange: Nothing
        , currentNNeighbors: ""
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

  sMouseLeft <- Signal.mouseButtonPressed Signal.MouseLeftButton
  sMouseRight <- Signal.mouseButtonPressed Signal.MouseRightButton
  sMousePos <- Signal.mousePos

  onEvent sMouseLeft $ \t -> when t $ do
    {x, y} <- Signal.get sMousePos
    modify_ (_ { mousePos = Tuple x y }) refAppState

  onEventE Signal.animationFrame $ \_ -> do
    -- check if resize is necessary
    canvasClientWidth <- clientWidth canvas
    canvasClientHeight <- clientHeight canvas
    canvasWidth <- getCanvasWidth canvasElement
    canvasHeight <- getCanvasHeight canvasElement
    unless (canvasClientWidth == canvasWidth) $ do
      setCanvasWidth canvasElement canvasClientWidth
      setCanvasWidth bufferElement canvasClientWidth
      setCanvasWidth uiElement canvasClientWidth
    unless (canvasClientHeight == canvasHeight) $ do
      setCanvasHeight canvasElement canvasClientHeight
      setCanvasHeight bufferElement canvasClientHeight
      setCanvasHeight uiElement canvasClientHeight

    -- copy current canvas to buffer
    -- TODO: check if better when apply scrolling here and apply rect-redraw to buffer
    clearRect bufferCtx { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }
    drawImage bufferCtx (canvasElementToImageSource canvasElement) 0.0 0.0

    app <- read refAppState

    -- redraw changes on buffer
    -- buggy
    render bufferCtx $ Drawing.redrawDelta app.changes
                                           canvasWidth canvasHeight
                                           app.viewPos
                                           app.zoomFactor
    -- clear changes cache
    flip modify_ refAppState $ \a ->
      a { frameCount = a.frameCount + 1
        , changes = []
        }

    -- copy buffer to canvas and apply scrolling
    clearRect ctx { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight}
    case app.scrollDelta of
      Nothing -> drawImage ctx (canvasElementToImageSource bufferElement) 0.0 0.0
      Just (Tuple deltaX deltaY) -> do
        let z = toNumber app.zoomFactor
        drawImage ctx (canvasElementToImageSource bufferElement) (toNumber deltaX) (toNumber deltaY)
        let viewPosNew = moveView z deltaX deltaY app.viewPos
        flip modify_ refAppState
          _ { viewPos = viewPosNew
            , scrollDelta = Nothing
            }

        -- TODO: full redraw on gaps caused by scrolling/zooming

        -- TODO: apply zoom scaling to buffer
        -- copy buffer image to canvas AND apply scroll translation to buffer
        -- deltaY > 0
        let width = canvasWidth / z
            height = canvasHeight / z
            Tuple viewX viewY = viewPosNew
            -- deltaY negative: moving up, empty space at bottom of screen
            --        positive: moving down, empty space at top of screen
            bottom1 =
              if deltaY < 0
                then viewY - height / 2.0 -- buggy
                else viewY + height / 2.0 - toNumber deltaY / z
            left1 = viewX - width / 2.0
            height1 = abs $ toNumber deltaY / z
        render ctx $ Drawing.redrawFull' Drawing.red app.gridState.cellStates
                                         canvasWidth canvasHeight
                                         left1 bottom1
                                         width height1
                                         viewPosNew
                                         app.zoomFactor
        let left2 =
              if deltaX < 0
                then viewX + width / 2.0 + toNumber deltaX / z
                else viewX - width / 2.0 -- buggy
            bottom2 =
              if deltaY < 0
                then viewY - height / 2.0 - toNumber deltaY / z
                else viewY - height / 2.0
            width2 = abs $ toNumber deltaX / z
            height2 = height - height1
        render ctx $ Drawing.redrawFull' Drawing.yellow app.gridState.cellStates
                                         canvasWidth canvasHeight
                                         left2 bottom2
                                         width2 height2
                                         viewPosNew
                                         app.zoomFactor
    app' <- read refAppState
    redrawUI app' canvasWidth canvasHeight uiCtx

  onEvent gridBeat $ \_ -> do
    flip modify_ refAppState \app ->
      app { frameRate = round $ 1000.0 * toNumber app.frameCount / gamePeriod
          , frameCount = 0
          }

  -- manual full redraw by pressing r key
  keyRDown <- keyPressed keyCodeR
  onEvent keyRDown \downUp -> when downUp $
    read refAppState >>= redrawFull canvasElement ctx

  -- manually advance state by pressing space bar
  spaceBarDown <- keyPressed keyCodeSpacebar
  onEvent spaceBarDown \downUp -> when downUp $ do
    width <- getCanvasWidth canvasElement
    height <- getCanvasHeight canvasElement
    flip modify_ refAppState \app ->
      let widthC = width / toNumber app.zoomFactor
          heightC = height / toNumber app.zoomFactor
          Tuple changes gridState = advance app.gridState widthC heightC app.viewPos
      in  app { changes = changes
              , gridState = gridState
              , frameRate = round $ 1000.0 * toNumber app.frameCount / gamePeriod
              , frameCount = 0
              }

  onEvent sMousePos $ \{x, y} -> do
    app <- read refAppState
    -- grid position label
    canvasWidth <- getCanvasWidth canvasElement
    canvasHeight <- getCanvasHeight canvasElement
    let Tuple viewX viewY = app.viewPos
        z = toNumber app.zoomFactor
        widthC = canvasWidth / z
        heightC = canvasHeight / z
        gridX = floor $ toNumber x / z - widthC / 2.0 + viewX
        gridY = floor $ heightC / 2.0 - toNumber y / z + viewY
        mouseVX = toNumber (floor $ (toNumber x / z - widthC / 2.0 + viewX) * 10.0) / 10.0
        mouseVY = toNumber (floor $ (heightC / 2.0 - toNumber y / z + viewY) * 10.0) / 10.0

    -- drag mouse to scroll
    whenM (Signal.get sMouseLeft) $ do
      let Tuple oldX oldY = app.mousePos
          Tuple dx dy = fromMaybe (Tuple 0 0) app.scrollDelta
      flip modify_ refAppState $
        _ { scrollDelta = Just $ Tuple (dx + x - oldX) (dy + y - oldY) }

    let gridIndex = gridX `mod` Grid.width + Grid.width * (gridY `mod` Grid.height)
    flip modify_ refAppState \a ->
      a { gridPos = Tuple gridX gridY
        , mouseVPos = Tuple mouseVX mouseVY
        , mousePos = Tuple x y
        , gridPosFromIndex = indexToViewCoords gridIndex a.viewPos widthC heightC
        , gridIndex = gridIndex
        , currentState = maybe "#" show $ (a.gridState.cellStates !! gridIndex)
        , currentChange = snd <$> find (\(Tuple i _) -> i == gridIndex) a.changes
        , currentNNeighbors = maybe "#" show $ (a.gridState.neighbors !! gridIndex)
        }

  onEvent sMouseRight $ \pressed -> when pressed $ do
    {x, y} <- Signal.get sMousePos
    canvasWidth <- getCanvasWidth canvasElement
    canvasHeight <- getCanvasHeight canvasElement
    flip modify_ refAppState $ \app ->
      let Tuple viewX viewY = app.viewPos
          z = toNumber app.zoomFactor
          widthC = canvasWidth / z
          heightC = canvasHeight / z
          gridX = floor $ toNumber x / z - widthC / 2.0 + viewX
          gridY = floor $ heightC / 2.0 - toNumber y / z + viewY
          gridIndex = gridX `mod` Grid.width + Grid.width * (gridY `mod` Grid.height)
          Tuple changes gridState = Grid.toggleCell app.gridState gridIndex
      in  app { changes = changes
              , gridState = gridState
              }

  redrawFull canvasElement ctx initialAppState


redrawUI :: AppState -> Number -> Number -> Context2D -> Effect Unit
redrawUI app width height uiCtx = do
  clearRect uiCtx { x: 0.0, y: 0.0, width, height }
  render uiCtx $ Drawing.redrawUI app.gridPosFromIndex
                                  app.currentState
                                  app.currentChange
                                  app.currentNNeighbors
                                  app.frameRate
                                  app.zoomFactor
                                  app.mousePos
                                  app.mouseVPos
                                  app.gridPos
                                  width height
                                  app.viewPos
                                  app.gridIndex

redrawFull :: CanvasElement -> Context2D -> AppState -> Effect Unit
redrawFull canvasElement canvasCtx app = do
  canvasWidth <- getCanvasWidth canvasElement
  canvasHeight <- getCanvasHeight canvasElement
  clearRect canvasCtx { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }
  let widthC = canvasWidth / toNumber app.zoomFactor
      heightC = canvasHeight / toNumber app.zoomFactor
      Tuple viewX viewY = app.viewPos
      left = viewX - widthC / 2.0
      bottom = viewY - heightC / 2.0
  render canvasCtx $ Drawing.redrawFull' Drawing.green app.gridState.cellStates
                                         canvasWidth canvasHeight
                                         left bottom
                                         widthC heightC
                                         app.viewPos
                                         app.zoomFactor

moveView :: Number -> Int -> Int -> Tuple Number Number -> Tuple Number Number
moveView z deltaX deltaY (Tuple vx vy) =
  let viewXMoved = vx - toNumber deltaX / z
      halfWidth = (if viewXMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.width / 2)
      viewXNew = (viewXMoved + halfWidth) % toNumber Grid.width - halfWidth

      viewYMoved = vy + toNumber deltaY / z
      halfHeight = (if viewYMoved > 0.0 then 1.0 else (-1.0)) * toNumber (Grid.height / 2)
      viewYNew = (viewYMoved + halfHeight) % toNumber Grid.height - halfHeight
  in  Tuple viewXNew viewYNew
