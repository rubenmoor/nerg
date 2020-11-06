module Drawing
 ( redrawGrid
 , Params
 ) where

import Prelude

import Color (black, fromInt, rgba, white)
import Data.Array (range)
import Data.Foldable (fold, foldMap)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, text)
import Graphics.Drawing.Font (font, monospace, sansSerif)
import Grid (CellState(..), LivingCells)
import Grid (width, height) as Grid
import Text.Formatting (int, number, print, s, string)

-- foreign import binarySearch :: forall a. Array (Tuple Int a) -> Int -> Maybe a
foreign import binarySearch_ :: forall a. (a -> Maybe a) -> Maybe a -> Array (Tuple Int a) -> Int -> Maybe a

binarySearch :: forall a. Array (Tuple Int a) -> Int -> Maybe a
binarySearch = binarySearch_ Just Nothing

type Params =
  { livingCells :: LivingCells
  , frameRate :: Int
  , canvasDims :: Tuple Int Int

  -- the coordinates of the real grid that the canvas is centered on
  -- (3, 3) means: centered on top-left corner of the (3, 3) square
  -- (0.5, 0.5) means: centered on the center of the (0, 0) square
  , viewPos :: Tuple Number Number

  -- number of pixels per cell
  , zoomFactor :: Int

  , mousePos :: Tuple Int Int
  , mouseVPos :: Tuple Number Number
  , gridPos :: Tuple Int Int
  }

redrawGrid :: Params -> Drawing
redrawGrid
       { livingCells
       , frameRate
       , canvasDims: Tuple canvasWidth canvasHeight
       , viewPos: Tuple viewX viewY
       , zoomFactor
       , mousePos: Tuple mouseX mouseY
       , mouseVPos: Tuple mouseVX mouseVY
       , gridPos: Tuple gridX gridY
       } =
       drawGridLines
    <> drawCells
    <> drawHoverFill
    <> drawCoordinateLabel 100 200
    <> drawCoordinateLabel (canvasWidth - 15) (canvasHeight - 15)
    <> drawFrameRate
    <> drawTooltip
    <> drawGridPos
    <> drawViewPos
    <> drawMouseViewPos
  where
    -- | scale transform y to cartesian coordinates
    sctr y = canvasHeight - y
    sctr' y = toNumber canvasHeight - y

    z = toNumber zoomFactor
    widthC = toNumber canvasWidth / z
    heightC = toNumber canvasHeight / z

    -- drawBackground =
      -- filled (fillColor black) $ rectangle 0.0 0.0 (toNumber canvasWidth) (toNumber canvasHeight)
    gray = fromInt 0x444444
    lightgray = fromInt 0x999999
    grayish = rgba 256 256 256 0.2

    drawCell row col state =
      let x = (widthC / 2.0 - viewX + toNumber col) * z
          y = (heightC / 2.0 - viewY + toNumber row) * z
          color = case state of
            Alive -> fromInt 0x1111FF
            Dead  -> black
      in  filled (fillColor color) $
               rectangle x
                         (sctr' y)
                         (z - 1.0)
                         (1.0 - z)

    drawCells =
      let left = floor (viewX - widthC / 2.0)
          right = floor (viewX + widthC / 2.0)
          bottom = floor (viewY - heightC / 2.0)
          top = floor (viewY + heightC / 2.0)
      in  flip foldMap (range bottom top) $ \row ->
            let toIndex col = (row `mod` Grid.height) * Grid.width + (col `mod` Grid.width)
            in  fold $ range left right <#> \col ->
                  case binarySearch livingCells (toIndex col) of
                    Just state -> drawCell row col state
                    Nothing    -> mempty

    toXCoords i =
      let xIndex = floor viewX
          xFraction = viewX - toNumber xIndex
          leftOffset = floor (toNumber canvasWidth / 2.0 - xFraction * z) `mod` zoomFactor
      in  leftOffset + i * zoomFactor

    toYCoords i =
      let yIndex = floor viewY
          yFraction = viewY - toNumber yIndex
          bottomOffset = floor (toNumber canvasHeight / 2.0 - yFraction * z) `mod` zoomFactor
      in  bottomOffset + i * zoomFactor

    xThickIndices = range 0 $ floor $ widthC / toNumber Grid.width
    toXIndices left i = (floor left `mod` Grid.width) + i * Grid.width
    yThickIndices = range 0 $ floor $ heightC / toNumber Grid.height
    toYIndices bottom i = (floor bottom `mod` Grid.height) + i * Grid.height

    drawGridLines =
      let -- thin lines
          toVertPaths = toNumber >>> \x ->
            path [ {x: x, y: 0.0}, {x: x, y: toNumber canvasHeight} ]
          toHoriPaths = sctr >>> toNumber >>> \y ->
            path [ {x: 0.0, y: y}, {x: toNumber canvasWidth, y: y} ]

          thinLines =
            let xIndices = range 0 $ floor widthC
                xLines = toVertPaths <<< toXCoords <$> xIndices

                yIndices = range 0 $ floor heightC
                yLines = toHoriPaths <<< toYCoords <$> yIndices
                thinStyle = lineWidth 1.0 <> outlineColor grayish
            in  outlined thinStyle (fold xLines <> fold yLines)

          -- thick lines
          nLeft = widthC / 2.0 - toNumber (Grid.width / 2) - viewX
          xBorderLines = toVertPaths <<< toXCoords <<< toXIndices nLeft <$> xThickIndices

          nBottom = heightC / 2.0 - toNumber (Grid.height / 2) - viewY
          yBorderLines = toHoriPaths <<< toYCoords <<< toYIndices nBottom <$> yThickIndices

          thickStyle = lineWidth 2.0 <> outlineColor gray
      in  (if zoomFactor >= 10 then thinLines else mempty)
            <> outlined thickStyle (fold xBorderLines <> fold yBorderLines)

    drawHoverFill =
      let left = widthC / 2.0 + toNumber gridX - viewX
          lefts = toXCoords <<< toXIndices left <$> xThickIndices
          bottom = heightC / 2.0 + toNumber gridY - viewY
          bottoms = toYCoords <<< toYIndices bottom <$> yThickIndices
          toRectangle l b = rectangle l (sctr' b) (z - 1.0) (1.0 - z)
          rects = fold $ lefts <#> \x ->
            fold $ bottoms <#> \y ->
              toRectangle (toNumber x) (toNumber y)
      in  filled (fillColor grayish) rects

    drawFrameRate =
      let myFont = font sansSerif 12 mempty
          style = fillColor lightgray
      in text myFont 5.0 15.0 style $ print (int <<< s " fps") frameRate

    drawCoordinateLabel x y =
      let myFont = font sansSerif 16 mempty
          style = fillColor white
          str = print (s "(" <<< int <<< s ", " <<< int <<< s ")") x y
          textX = if x < canvasWidth - 90 then x + 5 else x - 75
          textY = if y < canvasHeight - 25 then y + 20 else y - 10
      in drawXMark x y <> text myFont (toNumber textX) (toNumber textY) style str

    drawXMark a b =
      let x = toNumber a
          y = toNumber b
          style = lineWidth 1.0 <> outlineColor white
          stroke1 = path [ {x: x - 5.0, y: y - 5.0}, {x: x + 5.0, y: y + 5.0}]
          stroke2 = path [ {x: x - 5.0, y: y + 5.0}, {x: x + 5.0, y: y - 5.0}]
      in  outlined style $ stroke1 <> stroke2

    drawTooltip = mempty

    drawViewPos =
      let myFont = font monospace 12 mempty
          toPrecision x = toNumber (floor $ x * 10.0) / 10.0
      in  text myFont 5.0 (toNumber canvasHeight - 25.0)
               (fillColor white)
               (print (s "vx: " <<< number <<< s " | vy: " <<< number) (toPrecision viewX) (toPrecision viewY))

    drawMouseViewPos =
      let myFont = font monospace 12 mempty
      in  text myFont 5.0 (toNumber canvasHeight - 45.0)
               (fillColor white)
               (print (s "mx: " <<< number <<< s " | my: " <<< number) mouseVX mouseVY)

    drawGridPos =
      let myFont = font monospace 12 mempty
          x = if gridX > Grid.width / 2 - 1 || gridX < -Grid.width / 2 then "-" else show gridX
          y = if gridY > Grid.height / 2 - 1 || gridY < -Grid.height / 2 then "-" else show gridY
      in  text myFont 5.0  (toNumber canvasHeight - 5.0)
               (fillColor white)
               (print (s "x: " <<< string <<< s " | y: " <<< string) x y)
