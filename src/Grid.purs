module Grid
  ( width
  , height
  , PlayerId
  , CellState
  , CellOwner
  , GridState
  , Neighbors
  , emptyGrid
  , randomGrid
  ) where

import Prelude

import Control.Monad.ST (run)
import Data.Array (replicate)
import Data.Array.ST (freeze, modify, poke, thaw)
import Data.Foldable (class Foldable, foldr)
import Data.List.Lazy (List, foldM, replicateM, zipWith, (..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)

width :: Int
width = 100

height :: Int
height = 100

type PlayerId = Int

data CellState = Dead | Alive

-- not in use yet
data CellOwner = Neutral | Player PlayerId

type GridState = Array CellState
type Neighbors = Array Int

emptyGrid :: Tuple GridState Neighbors
emptyGrid =
  let length = width * height
  in  Tuple (replicate length Dead) (replicate length 0)

mapM_ :: forall m t a b. Monad m => Foldable t => (a -> m b) -> t a -> m Unit
mapM_ func xs = foldr acc (pure unit) xs
  where
    acc x action = func x >>= \_ -> action

forM_ :: forall m t a b. Monad m => Foldable t => t a -> (a -> m b) -> m Unit
forM_ = flip mapM_

foldM_ :: forall a m. Monad m => (a -> m Unit) -> List a -> m Unit
foldM_ action xs = foldM (\_ x -> action x) unit xs

forLoopM_ :: forall a m. Monad m => List a -> (a -> m Unit) -> m Unit
forLoopM_ = flip foldM_

randomGrid :: Number -> Effect (Tuple GridState Neighbors)
randomGrid density = do
  let length = width * height
  randoms <- zipWith Tuple (1..length) <$> replicateM length random
  pure $ run (do
    neighborCount <- thaw $ replicate length 0
    gridState <- thaw $ replicate length Dead
    forLoopM_ randoms $ \(Tuple i r) ->
      when (r < density) $ do
        _ <- poke i Alive gridState
        forM_ (neighborIndices i) $ \j ->
          modify i (\x -> x + 1) neighborCount

    Tuple <$> freeze gridState <*> freeze neighborCount
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
