module Grid
  ( width
  , height
  , PlayerId
  , CellState (..)
  , CellOwner (..)
  , LivingCells
  , CellStates
  , GridState
  , Neighbors
  , emptyGrid
  , randomGrid
  , foldM_
  , forLoopM_
  ) where

import Prelude

import Control.Monad.ST (run)
import Data.Array (replicate)
import Data.Array.ST (empty, freeze, modify, poke, push, thaw)
import Data.Foldable (class Foldable, foldM, foldr)
import Data.List.Lazy ((..))
import Data.List.Lazy (replicateM, zipWith) as Lazy
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)

width :: Int
width = 10

height :: Int
height = 10

type PlayerId = Int

data CellState = Dead | Alive

-- not in use yet
data CellOwner = Neutral | Player PlayerId

type CellStates = Array CellState
type Neighbors = Array Int

-- indices of living cells
type LivingCells = Array Int

type GridState =
  { livingCells :: LivingCells
  , cellStates :: CellStates
  , neighbors :: Neighbors
  }

emptyGrid :: GridState
emptyGrid =
  let length = width * height
  in  { livingCells: []
      , cellStates: replicate length Dead
      , neighbors: replicate length 0
      }

mapM_ :: forall m t a b. Monad m => Foldable t => (a -> m b) -> t a -> m Unit
mapM_ func xs = foldr acc (pure unit) xs
  where
    acc x action = func x >>= \_ -> action

forM_ :: forall m t a b. Monad m => Foldable t => t a -> (a -> m b) -> m Unit
forM_ = flip mapM_

foldM_ :: forall a m f. Monad m => Foldable f => (a -> m Unit) -> f a -> m Unit
foldM_ action xs = foldM (\_ x -> action x) unit xs

forLoopM_ :: forall a m f. Monad m => Foldable f => f a -> (a -> m Unit) -> m Unit
forLoopM_ = flip foldM_

randomGrid :: Number -> Effect GridState
randomGrid density = do
  let length = width * height
  randoms <- Lazy.zipWith Tuple (1..length) <$> Lazy.replicateM length random
  pure $ run (do
    neighbors <- thaw $ replicate length 0
    cellStates <- thaw $ replicate length Dead
    livingCells <- empty
    forLoopM_ randoms $ \(Tuple i r) ->
      when (r < density) $ do
        _ <- poke i Alive cellStates
        _ <- push i livingCells
        forM_ (neighborIndices i) $ \j ->
          modify i (\x -> x + 1) neighbors
    { livingCells: _, cellStates: _, neighbors: _} <$> freeze livingCells
                                                   <*> freeze cellStates
                                                   <*> freeze neighbors
    )

neighborIndices :: Int -> Array Int
neighborIndices i =
  let length = width * height
      leftMargin = if i `mod` width == 0 then width else 0
      rightMargin = if i + 1 `mod` width == 0 then width else 0
      n  = i - width `mod` length
      nw = n - 1 + leftMargin `mod` length
      ne = n + 1 - rightMargin `mod` length
      w = i - 1 + leftMargin `mod` length
      e = i + 1 - rightMargin `mod` length
      s = i + width `mod` length
      sw = s - 1 + leftMargin `mod` length
      se = s + 1 - rightMargin `mod` length
  in  [nw, n, ne, w, e, sw, s, se]
