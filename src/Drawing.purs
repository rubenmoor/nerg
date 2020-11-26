module Drawing
 ( redrawDelta
 , redrawFull
 , redrawUI
 ) where

import Prelude

import Color (black, fromInt, rgba, white)
import Data.Array (range, (!!))
import Data.Foldable (fold, foldMap)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Color, Drawing, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, text)
import Graphics.Drawing.Font (font, monospace, sansSerif)
import Grid (CellState(..), CellStates, Change(..), Changes)
import Grid (width, height) as Grid
import Text.Formatting (int, number, print, s, string)

-- foreign import binarySearch :: forall a. Array (Tuple Int a) -> Int -> Maybe a
foreign import binarySearch_ :: forall a. (a -> Maybe a) -> Maybe a -> Array (Tuple Int a) -> Int -> Maybe a

binarySearch :: forall a. Array (Tuple Int a) -> Int -> Maybe a
binarySearch = binarySearch_ Just Nothing

gray :: Color
gray = fromInt 0x444444

read :: Color
read = fromInt 0xFF0000

green :: Color
green = fromInt 0x00FF00

lightgray :: Color
lightgray = fromInt 0x999999

grayish :: Color
grayish = rgba 256 256 256 0.2

blue :: Color
blue = fromInt 0x1111FF

redrawDelta :: Changes -> Number -> Number -> Tuple Number Number -> Int -> Drawing
redrawDelta changes width height viewPos zoomFactor =
  flip foldMap changes $ \(Tuple i change) ->
    let z = toNumber zoomFactor
        vx = toNumber $ i `mod` Grid.width
        vy = toNumber $ i / Grid.width
        color = fillColor $ case change of
          Born -> blue
          Died -> black
    in  drawAtView width height zoomFactor viewPos vx vy $ \x y ->
          filled color $ rectangle (x + 1.0) (height - y - 1.0) (z - 2.0) (2.0 - z)

toXIndex :: Number -> Int -> Int
toXIndex left i = (floor left `mod` Grid.width) + i * Grid.width

toYIndex :: Number -> Int -> Int
toYIndex bottom i = (floor bottom `mod` Grid.height) + i * Grid.height

xThickIndices :: Number -> Array Int
xThickIndices widthC = range 0 $ floor $ widthC / toNumber Grid.width

yThickIndices :: Number -> Array Int
yThickIndices heightC = range 0 $ floor $ heightC / toNumber Grid.height

toXCoord :: Number -> Number -> Int -> Int -> Int
toXCoord viewX width zoomFactor i =
  let z = toNumber zoomFactor
      xIndex = floor viewX
      xFraction = viewX - toNumber xIndex
      leftOffset = floor (width / 2.0 - xFraction * z) `mod` zoomFactor
  in  leftOffset + i * zoomFactor

toYCoord :: Number -> Number -> Int -> Int -> Int
toYCoord viewY height zoomFactor i =
  let z = toNumber zoomFactor
      yIndex = floor viewY
      yFraction = viewY - toNumber yIndex
      bottomOffset = floor (height / 2.0 - yFraction * z) `mod` zoomFactor
  in  bottomOffset + i * zoomFactor

redrawFull' :: CellStates -> Number -> Number -> Number -> Number -> Tuple Number Number -> Int -> Drawing
redrawFull' cellStates left bottom width height (Tuple viewX viewY) zoomFactor =
  let z = toNumber zoomFactor

      drawGridLines =
        let -- thin lines
            toVertPaths = toNumber >>> \x ->
              path [ {x: x, y: 0.0}, {x: x, y: height} ]
            toHoriPaths = toNumber >>> \y ->
              path [ {x: 0.0, y: height - y}, {x: width, y: height - y} ]

            thinLines =
              let xIndices = range 0 $ floor width
                  xLines = toVertPaths <<< toXCoord viewX width zoomFactor <$> xIndices

                  yIndices = range 0 $ floor heightC
                  yLines = toHoriPaths <<< toYCoord viewY height zoomFactor <$> yIndices
                  thinStyle = lineWidth 1.0 <> outlineColor grayish
              in  outlined thinStyle (fold xLines <> fold yLines)

            -- thick lines
            nLeft = widthC / 2.0 - toNumber (Grid.width / 2) - viewX
            xBorderLines = toVertPaths <<< toXCoord viewX width zoomFactor <<< toXIndex nLeft <$> xThickIndices widthC

            nBottom = heightC / 2.0 - toNumber (Grid.height / 2) - viewY
            yBorderLines = toHoriPaths <<< toYCoord viewY height zoomFactor <<< toYIndex nBottom <$> yThickIndices heightC

            thickStyle = lineWidth 2.0 <> outlineColor gray
        in  (if zoomFactor >= 10 then thinLines else mempty)
            <> outlined thickStyle (fold xBorderLines <> fold yBorderLines)
  in  drawGridLines

redrawFull :: CellStates -> Number -> Number -> Tuple Number Number -> Int -> Drawing
redrawFull cellStates width height (Tuple viewX viewY) zoomFactor =
  let z = toNumber zoomFactor
      widthC = width / z
      heightC = height / z

      drawGridLines =
        let -- thin lines
            toVertPaths = toNumber >>> \x ->
              path [ {x: x, y: 0.0}, {x: x, y: height} ]
            toHoriPaths = toNumber >>> \y ->
              path [ {x: 0.0, y: height - y}, {x: width, y: height - y} ]

            thinLines =
              let xIndices = range 0 $ floor widthC
                  xLines = toVertPaths <<< toXCoord viewX width zoomFactor <$> xIndices

                  yIndices = range 0 $ floor heightC
                  yLines = toHoriPaths <<< toYCoord viewY height zoomFactor <$> yIndices
                  thinStyle = lineWidth 1.0 <> outlineColor grayish
              in  outlined thinStyle (fold xLines <> fold yLines)

            -- thick lines
            nLeft = widthC / 2.0 - toNumber (Grid.width / 2) - viewX
            xBorderLines = toVertPaths <<< toXCoord viewX width zoomFactor <<< toXIndex nLeft <$> xThickIndices widthC

            nBottom = heightC / 2.0 - toNumber (Grid.height / 2) - viewY
            yBorderLines = toHoriPaths <<< toYCoord viewY height zoomFactor <<< toYIndex nBottom <$> yThickIndices heightC

            thickStyle = lineWidth 2.0 <> outlineColor gray
        in  (if zoomFactor >= 10 then thinLines else mempty)
            <> outlined thickStyle (fold xBorderLines <> fold yBorderLines)

      -- TODO: explicit decision to use drawAtView on living cells
      -- currently looping through all of cellStates
      drawCells =
        let left = floor $ viewX - widthC / 2.0
            right = floor $ viewX + widthC / 2.0
            bottom = floor $ viewY - heightC / 2.0
            top = floor $ viewY + heightC / 2.0

        in  flip foldMap (range bottom top) $ \row ->
              let toIndex col = (row `mod` Grid.height) * Grid.width + (col `mod` Grid.width)
              in  fold $ range left right <#> \col ->
                    case cellStates !! toIndex col of
                      Just Alive ->
                        let x = (widthC / 2.0 - viewX + toNumber col) * z
                            y = (heightC / 2.0 - viewY + toNumber row) * z
                        in  filled (fillColor blue) $ rectangle x (height - y) (z - 1.0) (1.0 - z)
                      _          -> mempty

  in  drawGridLines <> drawCells

drawAtView :: Number -> Number -> Int -> Tuple Number Number -> Number -> Number -> (Number -> Number -> Drawing) -> Drawing
drawAtView width height zoomFactor (Tuple viewX viewY) vx vy drawFunc =
  let z = toNumber zoomFactor
      widthC = width / z
      heightC = height / z
      left = widthC / 2.0 + vx - viewX
      lefts = toXCoord viewX width zoomFactor <<< toXIndex left <$> xThickIndices widthC
      bottom = heightC / 2.0 + vy - viewY
      bottoms = toYCoord viewY height zoomFactor <<< toYIndex bottom <$> yThickIndices heightC
  in  fold $ lefts <#> \x ->
        fold $ bottoms <#> \y ->
          drawFunc (toNumber x) (toNumber y)

redrawUI :: Int -> Int -> Tuple Int Int -> Tuple Number Number -> Tuple Int Int -> Number -> Number -> Tuple Number Number -> Drawing
redrawUI frameRate zoomFactor (Tuple mouseX mouseY) (Tuple mouseVX mouseVY) (Tuple gridX gridY) width height viewPos@(Tuple viewX viewY) =
     drawHoverFill
  <> drawCoordinateLabel 100.1 200.0
  <> drawCoordinateLabel (width - 15.0) (height - 15.0)
  <> drawFrameRate
  <> drawTooltip
  <> drawGridPos
  <> drawViewPos
  <> drawMouseViewPos
  where
    z = toNumber zoomFactor
    widthC = width / z
    heightC = height / z

    drawHoverFill =
      drawAtView width height zoomFactor viewPos (toNumber gridX) (toNumber gridY) \x y ->
        filled (fillColor grayish) $
          rectangle x (height - y) (z - 1.0) (1.0 - z)

    drawFrameRate =
      let myFont = font sansSerif 12 mempty
          style = fillColor lightgray
      in text myFont 5.0 15.0 style $ print (int <<< s " fps") frameRate

    drawCoordinateLabel x y =
      let myFont = font sansSerif 16 mempty
          style = fillColor white
          str = print (s "(" <<< number <<< s ", " <<< number <<< s ")") x y
          textX = if x < width - 90.0 then x + 5.0 else x - 75.0
          textY = if y < height - 25.0 then y + 20.0 else y - 10.0
      in drawXMark x y <> text myFont textX textY style str

    drawXMark x y =
      let style = lineWidth 1.0 <> outlineColor white
          stroke1 = path [ {x: x - 5.0, y: y - 5.0}, {x: x + 5.0, y: y + 5.0}]
          stroke2 = path [ {x: x - 5.0, y: y + 5.0}, {x: x + 5.0, y: y - 5.0}]
      in  outlined style $ stroke1 <> stroke2

    drawTooltip = mempty

    drawViewPos =
      let myFont = font monospace 12 mempty
          toPrecision x = toNumber (floor $ x * 10.0) / 10.0
      in  text myFont 5.0 (height - 25.0)
               (fillColor white)
               (print (s "vx: " <<< number <<< s " | vy: " <<< number) (toPrecision viewX) (toPrecision viewY))

    drawMouseViewPos =
      let myFont = font monospace 12 mempty
      in  text myFont 5.0 (height - 45.0)
               (fillColor white)
               (print (s "mx: " <<< number <<< s " | my: " <<< number) mouseVX mouseVY)

    drawGridPos =
      let myFont = font monospace 12 mempty
          x = if gridX > Grid.width / 2 - 1 || gridX < -Grid.width / 2 then "-" else show gridX
          y = if gridY > Grid.height / 2 - 1 || gridY < -Grid.height / 2 then "-" else show gridY
      in  text myFont 5.0  (height - 5.0)
               (fillColor white)
               (print (s "x: " <<< string <<< s " | y: " <<< string) x y)
