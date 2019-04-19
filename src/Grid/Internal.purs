module Grid.Internal where

import Data.Array (filter, fromFoldable, replicate, zip, (..))
import Data.Function (($), (<<<))
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..), fst)
import Prelude (div, mod, (*), (+), (<))
import Random.PseudoRandom (mkSeed, randoms)

-- minimal cell state: dead or alive
-- can be later extended to contain more information
type CellState = Boolean

-- the grid is stored in a 1D-sequence
type Grid = Seq CellState

-- single cell for efficient rendering of changes only
type Cell =
  { x :: Int
  , y :: Int
  , state :: CellState
  }

type GridUpdate = Seq Cell

defaultCellState :: Boolean
defaultCellState = false

gridWidth :: Int
gridWidth = 100

gridHeight :: Int
gridHeight = 100

emptyGrid :: Grid
emptyGrid =
  Seq.fromFoldable $ replicate (gridWidth * gridHeight) defaultCellState

randomGrid :: Grid
randomGrid =
  let rs = randoms (gridWidth * gridHeight) (mkSeed 0)
      cs = map (\x -> if x < 0.2 then true else false) rs
  in  Seq.fromFoldable cs

-- calculate the 1D-index from x-y-coordinates
coordsToIndex :: Int -> Int -> Int
coordsToIndex x y = gridWidth * y + x

-- calculate the x-y-coordinates from the 1D-index
indexToCoords :: Int -> Tuple Int Int
indexToCoords i =
  let x = i `mod` gridWidth
      y = i `div` gridWidth
  in  Tuple x y

-- get the cell state at given coordinates
gridAt :: Grid -> Int -> Int -> Maybe CellState
gridAt grid x y = coordsToIndex x y `Seq.index` grid

-- set the cell state at given coordinates
gridSet :: Grid -> Int -> Int -> CellState -> Grid
gridSet grid x y cell = Seq.replace cell (coordsToIndex x y) grid

-- create a grid update that comprises all cells
wholeGridUpdate :: Grid -> GridUpdate
wholeGridUpdate grid =
  let indexedGrid = zip (0 .. (gridWidth * gridHeight)) $ fromFoldable grid
      activeIndices = filter (\(Tuple _ v) -> v) indexedGrid
      activeCoords = map (indexToCoords <<< fst) activeIndices
      activeCells = map (\(Tuple x y) -> {x, y, state: true}) activeCoords
  in  Seq.fromFoldable activeCells
