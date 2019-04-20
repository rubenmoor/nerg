module Data.Array.Extended
  ( module Data.Array
  , enumerate
  ) where

import Data.Array

import Data.Tuple (Tuple)
import Prelude ((-))

-- enumerate an array given the length
enumerate :: forall a. Int -> Array a -> Array (Tuple Int a)
enumerate n xs = zip (0 .. (n-1)) xs
