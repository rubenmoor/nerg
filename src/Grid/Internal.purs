module Grid.Internal where

import Data.Array (filter, fromFoldable, replicate, zip, (..))
import Data.Function (($), (<<<))
import Data.Functor (map)
import Data.Sequence.Extended (Seq)
import Data.Sequence.Extended as Seq
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst)
import Prelude (div, mod, (*), (+), (-), (<))
import Random.PseudoRandom (mkSeed, randoms)

-- minimal cell state: dead or alive
-- can be later extended to contain more information
type CellState = Boolean

-- the grid is stored in a 1D-sequence
type Grid = Seq CellInfo

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
  { x :: Int
  , y :: Int
  , state :: CellState
  }

type GridUpdate = Seq Cell

gridWidth :: Int
gridWidth = 100

gridHeight :: Int
gridHeight = 100

emptyGrid :: Grid
emptyGrid =
  Seq.fromFoldable $ replicate (gridWidth * gridHeight) defaultCellInfo

randomGrid :: Grid
randomGrid =
    foldl acc emptyGrid randomLs
  where
    acc :: Grid -> Tuple Int CellState -> Grid
    acc grid (Tuple i c) = if c then gridSet grid i c else grid

    randomLs :: Array (Tuple Int CellState)
    randomLs =
      let rnds = randoms (gridWidth * gridHeight) (mkSeed 0)
          rndStates = map (\x -> if x < 0.2 then true else false) rnds
      in  zip (0 .. (gridWidth * gridHeight - 1)) rndStates

-- calculate the x-y-coordinates from the 1D-index
indexToCoords :: Int -> Tuple Int Int
indexToCoords i =
  let x = i `mod` gridWidth
      y = i `div` gridWidth
  in  Tuple x y

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
      Seq.modify grid' i $ \oldCell ->
        oldCell { cellState = newState }

modifyNeighborCount :: (Int -> Int) -> Grid -> Int -> Grid
modifyNeighborCount f grid i =
  Seq.modify grid i $ \oldCell ->
    oldCell { nNeighbors = f oldCell.nNeighbors }

-- create a grid update that comprises all cells
wholeGridUpdate :: Grid -> GridUpdate
wholeGridUpdate grid =
  let indexedGrid = zip (0 .. (gridWidth * gridHeight)) $ fromFoldable grid
      activeIndices = filter (\(Tuple _ info) -> info.cellState) indexedGrid
      activeCoords = map (indexToCoords <<< fst) activeIndices
      activeCells = map (\(Tuple x y) -> {x, y, state: true}) activeCoords
  in  Seq.fromFoldable activeCells

neighborIndices :: Int -> Array Int
neighborIndices i =
  -- tbd. diagonals
  -- tbd. neighbors at margins
  let l = i - 1
      r = i + 1
      t = i - gridWidth
      b = i + gridWidth
  in  [l, r, t, b]
