module Grid
  ( width
  , height
  , advance
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
import Data.List.Lazy (replicateM, zipWith, range) as Lazy
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)

width :: Int
width = 100

height :: Int
height = 100

length :: Int
length = width * height

type PlayerId = Int

data CellState = Dead | Alive

-- not in use yet
data CellOwner = Neutral | Player PlayerId

type CellStates = Array CellState
type Neighbors = Array Int

data Change = Born | Died
type Changes = Array (Tuple Int Change)

type GridState =
  { changes :: Changes
  , cellStates :: CellStates
  , neighbors :: Neighbors
  }

emptyGrid :: GridState
emptyGrid =
  { changes: []
  , cellStates: replicate length Dead
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
    { changes: [], cellStates: _, neighbors: _}
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

advance :: CellStates -> Neighbors -> GridState
advance cs ns =
  run (do
    changes <- empty
    neighbors <- thaw ns
    cellStates <- thaw cs
    forLoopM_ (0..(length - 1)) $ \i -> do
      case cs !! i of
        Just Dead ->
          case ns !! i of
            Just 3 ->  do _ <- poke i Alive cellStates
                          addNeighbor neighbors i
                          void $ push (Tuple i Born) changes
            Just _ -> pure unit
            Nothing -> pure unit
        Just Alive ->
          case ns !! i of
            Just 2  -> pure unit
            Just 3  -> pure unit
            Just _  -> do _ <- poke i Dead cellStates
                          looseNeighbor neighbors i
                          void $ push (Tuple i Died) changes
            Nothing -> pure unit
        Nothing -> pure unit
    { changes: _, cellStates: _, neighbors: _}
      <$> freeze changes
      <*> freeze cellStates
      <*> freeze neighbors
  )
