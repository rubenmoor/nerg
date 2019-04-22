module Grid.Draw where

import Color (black, white)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)
import Grid.Internal (Cell, Grid, GridUpdate, gridHeight, gridWidth, wholeGridUpdate)
import Prelude (div, mod, (+), (/))

cellMargin :: Number
cellMargin = 0.0

-- redraw the entire grid by painting a black rectangle
-- and all active cells on top
redrawGrid :: Grid -> Drawing
redrawGrid grid =
  let bgRect = rectangle 0.0 0.0 (toNumber gridWidth) (toNumber gridHeight)
      background = filled (fillColor black) bgRect
  in  background <> drawUpdate (wholeGridUpdate grid)

-- redraw an update, i.e. a list of cells that need redrawing
-- due to change of status
drawUpdate :: GridUpdate -> Drawing
drawUpdate = foldMap drawCell

indexToCoords :: Int -> Tuple Int Int
indexToCoords i =
  let x = i `mod` gridWidth
      y = i `div` gridWidth
  in  Tuple x y

drawCell :: Cell -> Drawing
drawCell cell =
  let Tuple x y = indexToCoords cell.i
      Tuple color rect = case cell.state of
        true  -> let left = toNumber x + cellMargin / 2.0
                     top = toNumber y + cellMargin / 2.0
                     rect = rectangle left top 1.0 1.0
                 in  Tuple white rect
        false -> let left = toNumber x
                     top = toNumber y
                     rect = rectangle left top 1.0 1.0
                 in  Tuple black rect
      in  filled (fillColor color) rect
