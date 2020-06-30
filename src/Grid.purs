module Grid
  ( module Grid
  ) where

import Data.Array (foldl)
import Data.Array.Extended (enumerate)
import Data.BooleanAlgebra ((||))
import Data.List (List (..))
import Data.Eq ((==))
import Data.Tuple (Tuple(..))
import Grid.Internal (CellInfo, Grid, GridUpdate, emptyGridUpdate, gridHeight, gridSet, gridWidth)
import Prelude ((*), (<), (>))

nextState :: Grid -> Tuple GridUpdate Grid
nextState oldGrid =
    foldl acc (Tuple emptyGridUpdate oldGrid) (enumerate (gridWidth * gridHeight) oldGrid)
  where
    acc :: Tuple GridUpdate Grid -> Tuple Int CellInfo -> Tuple GridUpdate Grid
    acc unchanged@(Tuple update grid) (Tuple i ci) =
        if ci.cellState
           then if ci.nNeighbors < 2 || ci.nNeighbors > 3
                   then change false
                   else unchanged
           else if ci.nNeighbors == 3
                   then change true
                   else unchanged
      where
        change :: Boolean -> Tuple GridUpdate Grid
        change state =
          let update' = Cons {i, state} update
              grid'   = gridSet grid i state
          in  Tuple update' grid'
