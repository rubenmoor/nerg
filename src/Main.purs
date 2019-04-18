module Main where

import Prelude

import Color (black, white)
import Data.Array (replicate, zip, (..), fromFoldable, filter)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Tuple (Tuple (..), fst)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, render, fillColor, filled, rectangle)
import Effect.Console as Console
import Random.PseudoRandom (randoms, mkSeed)

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= case _ of
      Nothing -> Console.log ("canvas not found.")
      Just canvas -> do
        ctx <- getContext2D canvas
        render ctx drawing

drawing :: Drawing
drawing = redrawGrid randomGrid

type CellState = Boolean
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

coordsToIndex :: Int -> Int -> Int
coordsToIndex x y = gridWidth * y + x

indexToCoords :: Int -> Tuple Int Int
indexToCoords i =
  let x = i `mod` gridWidth
      y = i `div` gridWidth
  in  Tuple x y

gridAt :: Grid -> Int -> Int -> Maybe CellState
gridAt grid x y = coordsToIndex x y `Seq.index` grid

gridSet :: Grid -> Int -> Int -> CellState -> Grid
gridSet grid x y cell = Seq.replace cell (coordsToIndex x y) grid

cellMargin :: Number
cellMargin = 1.0

scalingFactor :: Number
scalingFactor = 10.0

redrawGrid :: Grid -> Drawing
redrawGrid grid =
  let bgRect = rectangle 0.0 0.0 640.0 640.0
      background = filled (fillColor black) bgRect
  in  background <> drawUpdate (wholeGridUpdate grid)

wholeGridUpdate :: Grid -> GridUpdate
wholeGridUpdate grid =
  let indexedGrid = zip (0 .. (gridWidth * gridHeight)) $ fromFoldable grid
      activeIndices = filter (\(Tuple _ v) -> v) indexedGrid
      activeCoords = map (indexToCoords <<< fst) activeIndices
      activeCells = map (\(Tuple x y) -> {x, y, state: true}) activeCoords
  in  Seq.fromFoldable activeCells

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
