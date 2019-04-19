module Grid.Render where

import Color (black, white)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Monoid ((<>))
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)
import Grid.Internal (Grid, GridUpdate, Cell, wholeGridUpdate)
import Prelude ((*), (+), (/))

cellMargin :: Number
cellMargin = 1.0

scalingFactor :: Number
scalingFactor = 10.0

-- redraw the entire grid by painting a black rectangle
-- and all active cells on top
redrawGrid :: Grid -> Drawing
redrawGrid grid =
  let bgRect = rectangle 0.0 0.0 640.0 640.0
      background = filled (fillColor black) bgRect
  in  background <> drawUpdate (wholeGridUpdate grid)

-- redraw an update, i.e. a list of cells that need redrawing
-- due to change of status
drawUpdate :: GridUpdate -> Drawing
drawUpdate = foldMap drawCell

drawCell :: Cell -> Drawing
drawCell cell =
  let left = toNumber cell.x * scalingFactor + cellMargin / 2.0
      top = toNumber cell.y * scalingFactor + cellMargin / 2.0
      rect = rectangle left top scalingFactor scalingFactor
      color = case cell.state of
        true -> white
        false -> black
  in  filled (fillColor color) rect
