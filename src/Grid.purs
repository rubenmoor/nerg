module Grid
  ( width
  , height
  , advance
  , indexToViewCoords
  , PlayerId
  , CellState (..)
  , CellOwner (..)
  , Changes
  , Change (..)
  , CellStates
  , GridState
  , Neighbors
  , emptyGrid
  , randomGrid
  , foldM_
  , forLoopM_
  ) where

import Prelude

import Control.Monad.ST (ST, run)
import Data.Array (replicate, (!!), (..))
import Data.Array.ST (STArray, empty, freeze, modify, poke, push, thaw)
import Data.Foldable (class Foldable, foldM, foldr)
import Data.Int (ceil, floor, toNumber)
import Data.List.Lazy (replicateM, zipWith, range) as Lazy
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Random (random)

foreign import sign :: Number -> Int

width :: Int
width = 20

height :: Int
height = 20

length :: Int
length = width * height

type PlayerId = Int

data CellState = Dead | Alive

instance showCellState :: Show CellState where
  show Dead = "dead"
  show Alive = "alive"

-- not in use yet
data CellOwner = Neutral | Player PlayerId

type CellStates = Array CellState
type Neighbors = Array Int

data Change = Born | Died
instance showChange :: Show Change where
  show Born = "born"
  show Died = "died"

type Changes = Array (Tuple Int Change)

type GridState =
  { cellStates :: CellStates
  , neighbors :: Neighbors
  }

emptyGrid :: GridState
emptyGrid =
  { cellStates: replicate length Dead
  , neighbors: replicate length 0
  }

mapM_ :: forall m t a. Monad m => Foldable t => (a -> m Unit) -> t a -> m Unit
mapM_ func xs = foldr acc (pure unit) xs
  where
    acc x action = func x >>= \_ -> action

forM_ :: forall m t a. Monad m => Foldable t => t a -> (a -> m Unit) -> m Unit
forM_ = flip mapM_

foldM_ :: forall a m f. Monad m => Foldable f => (a -> m Unit) -> f a -> m Unit
foldM_ action xs = foldM (\_ x -> action x) unit xs

forLoopM_ :: forall a m f. Monad m => Foldable f => f a -> (a -> m Unit) -> m Unit
forLoopM_ = flip foldM_

randomGrid :: Number -> Effect GridState
randomGrid density = do
  randoms <- Lazy.zipWith Tuple (Lazy.range 0 $ length - 1) <$> Lazy.replicateM length random
  pure $ run (do
    neighbors <- thaw $ replicate length 0
    cellStates <- thaw $ replicate length Dead
    forLoopM_ randoms $ \(Tuple i r) ->
      when (r < density) $ do
        _ <- poke i Alive cellStates
        addNeighbor neighbors i
    { cellStates: _, neighbors: _}
      <$> freeze cellStates
      <*> freeze neighbors
    )

neighborIndices :: Int -> Array Int
neighborIndices i =
  let leftMargin = if i `mod` width == 0 then width else 0
      rightMargin = if (i + 1) `mod` width == 0 then width else 0
      n  = (i - width) `mod` length
      nw = (n - 1 + leftMargin) `mod` length
      ne = (n + 1 - rightMargin) `mod` length
      w = (i - 1 + leftMargin) `mod` length
      e = (i + 1 - rightMargin) `mod` length
      s = (i + width) `mod` length
      sw = (s - 1 + leftMargin) `mod` length
      se = (s + 1 - rightMargin) `mod` length
  in  [nw, n, ne, w, e, sw, s, se]

addNeighbor :: forall h. STArray h Int -> Int -> ST h Unit
addNeighbor ns i =
  forM_ (neighborIndices i) $ \j ->
    void $ modify j (\x -> x + 1) ns

looseNeighbor :: forall h. STArray h Int -> Int -> ST h Unit
looseNeighbor ns i =
  forM_ (neighborIndices i) $ \j ->
    void $ modify j (\x -> x - 1) ns

wrap :: Int -> Int -> Int -> Int
wrap n offset x = (x - n / 2 - offset) `mod` n - n / 2 + offset
-- wrap n _ x = (x - n / 2) `mod` n - n / 2

indexToViewCoords :: Int -> Tuple Number Number -> Number -> Number -> Tuple Int Int
indexToViewCoords i (Tuple viewX viewY) widthC heightC =
  let offsetX = sign viewX * (ceil (max 0.0 (widthC / 2.0 + viewX - toNumber (width / 2)) +
                                    max 0.0 (widthC / 2.0 - viewX - toNumber (width / 2))) `mod` width)
      offsetY = sign viewY * (ceil (max 0.0 (heightC / 2.0 + viewY - toNumber (height / 2)) +
                                    max 0.0 (heightC / 2.0 - viewY - toNumber (height / 2))) `mod` height)
      x = i `mod` width
      y = i / width
  in  Tuple (wrap width offsetX x) (wrap height offsetY y)

getOffset :: Number -> Number -> Int -> Int
getOffset viewPos widthHeightC widthHeight =
  sign viewPos * (ceil (max 0.0 (widthHeightC / 2.0 + viewPos - toNumber (widthHeight / 2)) + max 0.0 (widthHeightC / 2.0 - viewPos - toNumber (widthHeight / 2))) `mod` widthHeight)

advance :: GridState -> Number -> Number -> Tuple Number Number -> Tuple Changes GridState
advance ({ cellStates: oldCellStates, neighbors: oldNeighbors })
        widthC heightC view@(Tuple viewX viewY) =
  -- TODO: changes are not not in sync
  run (do
    let left = floor (viewX - widthC / 2.0)
        right = floor (viewX + widthC / 2.0)
        bottom = floor (viewY - heightC / 2.0)
        top = floor (viewY + heightC / 2.0)
    changes <- trace (show [left, right, bottom, top]) \_ -> empty
    cellStates <- thaw (oldCellStates :: CellStates)
    neighbors <- thaw (oldNeighbors :: Neighbors)
    forLoopM_ (0..(length - 1)) \i ->
      case oldCellStates !! i of
        Just Dead ->
          case oldNeighbors !! i of
            Just 3 ->  do _ <- poke i Alive cellStates
                          addNeighbor neighbors i
                          let Tuple x y = indexToViewCoords i view widthC heightC
                          when (x > left && x < right && y >= bottom && y <= top) $
                            void $ push (Tuple i Born) changes
            Just _ -> pure unit
            Nothing -> trace "oldNeighbors index out of bounds" \_ -> pure unit
        Just Alive ->
          case oldNeighbors !! i of
            Just 2  -> pure unit
            Just 3  -> pure unit
            Just _  -> do _ <- poke i Dead cellStates
                          looseNeighbor neighbors i
                          let Tuple x y = indexToViewCoords i view widthC heightC
                          when (x > left && x < right && y >= bottom && y <= top) $
                            void $ push (Tuple i Died) changes
            Nothing -> trace "oldNeighbors index out of bounds (2)" \_ -> pure unit
        Nothing -> trace "oldCellstates index out of bounds" \_ -> pure unit
    Tuple <$> freeze changes
          <*> ({ cellStates: _, neighbors: _}
                <$> freeze cellStates
                <*> freeze neighbors
              )
  )
