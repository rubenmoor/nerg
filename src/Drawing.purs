module Drawing
 ( redraw
 , Params
 ) where

import Prelude

import Color (black, fromInt, white)
import Data.Array (foldMap, range)
import Data.Int (floor, round, toNumber)
import Graphics.Drawing (Drawing, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, text)
import Graphics.Drawing.Font (font, sansSerif)
import Text.Formatting (int, print, s)

type Params =
  { width :: Int
  , height :: Int

  -- the coordinates of the real grid that the canvas is centered on
  -- (3, 3) means: centered on top-left corner of the (3, 3) square
  -- (0.5, 0.5) means: centered on the center of the (0, 0) square
  , viewX :: Number
  , viewY :: Number

  , zoomFactor :: Int
  }

redraw :: Params -> Drawing
redraw { width: canvasWidth
       , height: canvasHeight
       , viewX: viewX
       , viewY: viewY
       , zoomFactor: zoomFactor
       } =
     drawBackground
  <> drawGrid
  <> drawCoordinateLabel 10 10
  <> drawCoordinateLabel (canvasWidth - 15) (canvasHeight - 15)
  where
    drawBackground =
      filled (fillColor black) $ rectangle 0.0 0.0 (toNumber canvasWidth) (toNumber canvasHeight)

    drawGrid =
      let gray = (fromInt 0x919191)
          style = lineWidth 1.0 <> outlineColor gray
          xIndex = floor viewX
          yIndex = floor viewY
          xFraction = viewX - toNumber xIndex
          yFraction = viewY - toNumber yIndex
          xOffset = (canvasWidth / 2 - round (xFraction * toNumber zoomFactor)) `mod` zoomFactor
          yOffset = (canvasHeight / 2 - round (yFraction * toNumber zoomFactor)) `mod` zoomFactor
          xArray = if xOffset <= canvasWidth then range 0 $ canvasWidth / zoomFactor else []
          yArray = if yOffset <= canvasHeight then range 0 $ canvasHeight / zoomFactor else []
          xs = xArray <#> \i -> xOffset + i * zoomFactor
          ys = yArray <#> \i -> yOffset + i * zoomFactor
          xLines = foldMap (toNumber >>> \x -> path [ {x: x, y: 0.0}, {x: x, y: toNumber canvasHeight} ]) xs
          yLines = foldMap (toNumber >>> \y -> path [ {x: 0.0, y: y}, {x: toNumber canvasWidth, y: y} ]) ys
      in outlined style $ xLines <> yLines

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
