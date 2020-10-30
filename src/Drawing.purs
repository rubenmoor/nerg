module Drawing
 ( redraw
 , Params
 ) where

import Prelude

import Color (black, fromInt, rgba, white)
import Data.Array (foldMap, range)
import Data.Int (ceil, floor, round, toNumber)
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, text)
import Graphics.Drawing.Font (font, monospace, sansSerif)
import Grid (GridState)
import Grid (width, height) as Grid
import Text.Formatting (int, number, print, s, string)

type Params =
  { gridState :: GridState
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

redraw :: Params -> Drawing
redraw { gridState: gridState
       , frameRate: frameRate
       , canvasDims: Tuple canvasWidth canvasHeight
       , viewPos: Tuple viewX viewY
       , zoomFactor: zoomFactor
       , mousePos: Tuple mouseX mouseY
       , mouseVPos: Tuple mouseVX mouseVY
       , gridPos: Tuple gridX gridY
       } =
     drawBackground
  <> drawCells
  <> drawGridLines
  <> drawHoverFill
  <> drawCoordinateLabel 100 200
  <> drawCoordinateLabel (canvasWidth - 15) (canvasHeight - 15)
  <> drawFrameRate
  <> drawTooltip
  <> drawGridPos
  <> drawViewPos
  <> drawMouseViewPos
  where
    drawBackground =
      filled (fillColor black) $ rectangle 0.0 0.0 (toNumber canvasWidth) (toNumber canvasHeight)

    gray = fromInt 0x919191
    darkgray = rgba 256 256 256 0.1

    drawCells = mempty -- foldMap (case _ of Alive -> drawCell Dead -> mempty) gridState

    drawGridLines =
      let
          style = lineWidth 0.5 <> outlineColor gray
          xIndex = floor viewX -- index of first border left of canvas center
          yIndex = floor viewY -- index of first border bottom of canvas center
          xFraction = viewX - toNumber xIndex
          yFraction = viewY - toNumber yIndex
          leftOffset = (canvasWidth / 2 - round (xFraction * toNumber zoomFactor)) `mod` zoomFactor
          -- rightOffset = canvasWidth - leftOffset `mod` zoomFactor
          topOffset = (canvasHeight / 2 - round (yFraction * toNumber zoomFactor)) `mod` zoomFactor
          -- bottomOffset = canvasHeight - topOffset `mod` zoomFactor
          -- nXOverflowR = floor (toNumber nVerticalLines / 2.0 + viewX) - Grid.width
          -- nXOverflowR = max 0 $ nVerticalLines - Grid.width - 1
          -- TODO check if factor in zoom factor
          left = canvasWidth / 2 - round ((toNumber (Grid.width / 2) + viewX) * toNumber zoomFactor)
          right = canvasWidth / 2 - round ((toNumber (Grid.width / 2) - viewX) * toNumber zoomFactor)
          nLeft = max 0 $ left / zoomFactor
          nRight = max 0 $ right / zoomFactor
          -- nOverflowL = floor (toNumber nVerticalLines / 2.0 - viewX) - Grid.width / 2
          -- render limited grid
          xArray = range nLeft $ canvasWidth / zoomFactor - nRight - 1
          top = canvasHeight / 2 - round ((toNumber (Grid.height / 2) + viewY) * toNumber zoomFactor)
          bottom = canvasHeight / 2 - round ((toNumber (Grid.height / 2) - viewY) * toNumber zoomFactor)
          nTop = max 0 $ top / zoomFactor
          nBottom = max 0 $ bottom / zoomFactor
          yArray = range nTop $ canvasHeight / zoomFactor - nBottom - 1
          xs = xArray <#> \i -> leftOffset + i * zoomFactor
          ys = yArray <#> \i -> topOffset + i * zoomFactor
          xLines = foldMap (toNumber >>> \x -> path [ {x: x, y: 0.0}, {x: x, y: toNumber canvasHeight} ]) xs
          yLines = foldMap (toNumber >>> \y -> path [ {x: 0.0, y: y}, {x: toNumber canvasWidth, y: y} ]) ys
      in outlined style $ xLines <> yLines

    drawHoverFill =
      let z = toNumber zoomFactor
          cellLeft = (toNumber gridX - viewX) * z + toNumber canvasWidth / 2.0
          cellTop = toNumber canvasHeight / 2.0 - (toNumber gridY + viewY) * z
      in  filled (fillColor darkgray) $ rectangle cellLeft cellTop (z - 1.0) (z - 1.0)

    drawFrameRate =
      let myFont = font sansSerif 12 mempty
          style = fillColor gray
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
      in  text myFont 5.0  (toNumber canvasHeight - 25.0)
               (fillColor white)
               (print (s "vx: " <<< number <<< s " | vy: " <<< number) (toPrecision viewX) (toPrecision viewY))
    drawMouseViewPos =
      let myFont = font monospace 12 mempty
      in  text myFont 5.0  (toNumber canvasHeight - 45.0)
               (fillColor white)
               (print (s "mx: " <<< number <<< s " | my: " <<< number) mouseVX mouseVY)
    drawGridPos =
      let myFont = font monospace 12 mempty
          x = if gridX > Grid.width / 2 - 1 || gridX < -Grid.width / 2 then "-" else show gridX
          y = if gridY > Grid.height / 2 - 1 || gridY < -Grid.height / 2 then "-" else show gridY
      in  text myFont 5.0  (toNumber canvasHeight - 5.0)
               (fillColor white)
               (print (s "x: " <<< string <<< s " | y: " <<< string) x y)
