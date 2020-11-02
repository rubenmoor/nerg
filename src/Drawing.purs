module Drawing
 ( redraw
 , Params
 ) where

import Prelude

import Color (black, fromInt, rgba, white)
import Data.Array (range, (!!))
import Data.Foldable (fold, foldMap)
import Data.Int (ceil, floor, toNumber)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, text)
import Graphics.Drawing.Font (font, monospace, sansSerif)
import Grid (CellState(..), GridState)
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
    <> drawGridLines
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

    drawBackground =
      filled (fillColor black) $ rectangle 0.0 0.0 (toNumber canvasWidth) (toNumber canvasHeight)

    gray = fromInt 0x444444
    lightgray = fromInt 0x999999
    grayish = rgba 256 256 256 0.2

    drawCell row col (Just Dead) = mempty
    drawCell row col mState =
      let x = (widthC / 2.0 - toNumber (Grid.width / 2) - viewX + toNumber col) * z
          y = (heightC / 2.0 - toNumber (Grid.height / 2) - viewY + toNumber row) * z
          color = case mState of
                       Just Alive -> fromInt 0x1111FF
                       _ -> fromInt 0xFF0000
      in  filled (fillColor color) $
               rectangle x
                         (sctr' y)
                         (z - 1.0)
                         (1.0 - z)

    drawCells =
      let left = floor (viewX - widthC / 2.0)
          right = 1 + ceil (viewX + widthC / 2.0)
          bottom = floor (viewY - heightC / 2.0)
          top = 1 + ceil (viewY + heightC / 2.0)
          -- at zoom factor 7, canvas size 300px x 300px:
          -- * JS Array: 44/45 fps, crash when decreasing zoom
          -- * Lazy list/lists: 4/5 fps, no crash
          visibleCellIndices =
            fold $ range bottom top <#> \row ->
              range left right <#> \col ->
                Tuple row col
          stateAt row col =
            let i = (row `mod` Grid.height) * Grid.width + (col `mod` Grid.width)
            in  gridState !! i
      in  flip foldMap visibleCellIndices $ \(Tuple row col) ->
            drawCell row col $ stateAt row col

    drawGridLines =
      let -- vertical lines
          xIndex = floor viewX -- index of first border left of canvas center
          xFraction = viewX - toNumber xIndex
          leftOffset = floor (toNumber canvasWidth / 2.0 - xFraction * z) `mod` zoomFactor

          -- thin lines
          xIndices = range 0 $ floor widthC
          toVertPaths = toNumber >>> \x ->
            path [ {x: x, y: 0.0}, {x: x, y: toNumber canvasHeight} ]
          toXCoords i = leftOffset + i * zoomFactor
          xLines = toVertPaths <<< toXCoords <$> xIndices

          -- thick lines
          xBorderIndices = range 0 $ floor $ widthC / toNumber Grid.width
          nLeft = floor $ widthC / 2.0 - toNumber (Grid.width / 2) - viewX
          toXIndices i = (nLeft `mod` Grid.width) + i * Grid.width
          xBorderLines = toVertPaths <<< toXCoords <<< toXIndices <$> xBorderIndices

          -- horizontal lines
          yIndex = floor viewY -- index of first border bottom of canvas center
          yFraction = viewY - toNumber yIndex
          bottomOffset = floor (toNumber canvasHeight / 2.0 - yFraction * z) `mod` zoomFactor

          -- thin lines
          yIndices = range 0 $ floor heightC
          toHoriPaths = sctr >>> toNumber >>> \y -> path [ {x: 0.0, y: y}, {x: toNumber canvasWidth, y: y} ]
          toYCoords i = bottomOffset + i * zoomFactor
          yLines = toHoriPaths <<< toYCoords <$> yIndices

          -- thick lines
          yBorderIndices = range 0 $ floor $ heightC / toNumber Grid.height
          nBottom = floor $ heightC / 2.0 - toNumber (Grid.height / 2) - viewY
          toYIndices i = (nBottom `mod` Grid.height) + i * Grid.height
          yBorderLines = toHoriPaths <<< toYCoords <<< toYIndices <$> yBorderIndices

          thinStyle = lineWidth 1.0 <> outlineColor grayish
          thickStyle = lineWidth 2.0 <> outlineColor gray
      in    outlined thinStyle (fold xLines <> fold yLines)
         <> outlined thickStyle (fold xBorderLines <> fold yBorderLines)

    drawHoverFill =
      let cellLeft = (toNumber gridX - viewX) * z + toNumber canvasWidth / 2.0
          cellTop = sctr' $ (toNumber gridY - viewY) * z + toNumber canvasHeight / 2.0
      in  filled (fillColor grayish) $ rectangle cellLeft cellTop (z - 1.0) (1.0 - z)

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
