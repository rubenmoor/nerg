module Grid.Draw where

import Color (black, white)
import Data.Foldable (foldMap)
import Data.Function (($))
import Data.Int (toNumber)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)
import Grid.Internal (Cell, Grid, GridUpdate, gridWidth, wholeGridUpdate)
import Prelude (div, mod, (*), (+))

cellMargin :: Int
cellMargin = 1

-- redraw the entire grid by painting a black rectangle
-- and all active cells on top
redrawGrid :: Grid -> Drawing
redrawGrid grid =
  let bgRect = rectangle 0.0 0.0 1000.0 1000.0
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
        true  -> let left = toNumber $ x * 10 + cellMargin
                     top = toNumber $ y * 10 + cellMargin
                     rect = rectangle left top 8.0 8.0
                 in  Tuple white rect
        false -> let left = toNumber $ x * 10
                     top = toNumber $ y * 10
                     rect = rectangle left top 10.0 10.0
                 in  Tuple black rect
      in  filled (fillColor color) rect
