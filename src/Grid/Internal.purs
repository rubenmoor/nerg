module Grid.Internal where

import Data.Array.Extended (enumerate, filter, modifyAt, replicate)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (map)
import Data.List (List)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst)
import Prelude (mod, (*), (+), (-), (<))
import Random.PseudoRandom (mkSeed, randoms)

-- minimal cell state: dead or alive
-- can be later extended to contain more information
type CellState = Boolean

-- the grid is stored in a 1D-sequence
type Grid = Array CellInfo

type CellInfo =
  { nNeighbors :: Int
  , cellState :: CellState
  }

defaultCellInfo :: CellInfo
defaultCellInfo =
  { nNeighbors: 0
  , cellState: false
  }

-- single cell for efficient rendering of changes only
type Cell =
  { i :: Int
  , state :: CellState
  }

type GridUpdate = List Cell

emptyGridUpdate :: GridUpdate
emptyGridUpdate = List.Nil

gridWidth :: Int
gridWidth = 100

gridHeight :: Int
gridHeight = 100

emptyGrid :: Grid
emptyGrid =
  replicate (gridWidth * gridHeight) defaultCellInfo

randomGrid :: Grid
randomGrid =
    foldl acc emptyGrid randomLs
  where
    acc :: Grid -> Tuple Int CellState -> Grid
    acc grid (Tuple i c) = if c then gridSet grid i c else grid

    randomLs :: Array (Tuple Int CellState)
    randomLs =
      let rnds = randoms (gridWidth * gridHeight) (mkSeed 0)
          rndStates = map (\x -> if x < 0.4 then true else false) rnds
      in  enumerate (gridWidth * gridHeight) rndStates

-- set the cell state at given coordinates
gridSet :: Grid -> Int -> CellState -> Grid
gridSet grid i newState =
   updateNeighbors $ updateState grid
  where
    updateNeighbors :: Grid -> Grid
    updateNeighbors grid' =
        foldl (modifyNeighborCount f) grid' (neighborIndices i)
      where
        f = if newState then (\x -> x + 1) else (\x -> x - 1)

    updateState :: Grid -> Grid
    updateState grid' =
      fromMaybe grid' $ modifyAt i (\c -> c { cellState = newState }) grid'

    modifyNeighborCount :: (Int -> Int) -> Grid -> Int -> Grid
    modifyNeighborCount f grid' i' =
        fromMaybe grid' $ modifyAt i' updateCell grid'
      where
        updateCell c = c { nNeighbors = f c.nNeighbors }

-- create a grid update that comprises all cells
wholeGridUpdate :: Grid -> GridUpdate
wholeGridUpdate grid =
  let indexedGrid = enumerate (gridWidth * gridHeight) grid
      activeIndices = filter (\(Tuple _ info) -> info.cellState) indexedGrid
      activeCoords = map fst activeIndices
      activeCells = map (\i -> {i, state: true}) activeCoords
  in  List.fromFoldable activeCells

-- logically, neighbor indices are easier to compute based on the
-- 2D-x-y-coordinates, but the index-based calculation is computationally
-- more effcient; it avoids two coord-conversions
neighborIndices :: Int -> Array Int
neighborIndices i =
  let length = gridWidth * gridHeight
      leftMargin = if i `mod` gridWidth == 0 then gridWidth else 0
      rightMargin = if i + 1 `mod` gridWidth == 0 then gridWidth else 0
      n  = i - gridWidth `mod` length
      nw = n - 1 + leftMargin `mod` length
      ne = n + 1 - rightMargin `mod` length
      w = i - 1 + leftMargin `mod` length
      e = i + 1 - rightMargin `mod` length
      s = i + gridWidth `mod` length
      sw = s - 1 + leftMargin `mod` length
      se = s + 1 - rightMargin `mod` length
  in  [nw, n, ne, w, e, sw, s, se]
