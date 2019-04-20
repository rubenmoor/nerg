module Data.Sequence.Extended
  ( module Data.Sequence
  , modify
  )
  where

import Data.Sequence

import Data.Maybe (Maybe(..))

modify :: forall a. Seq a -> Int -> (a -> a) -> Seq a
modify s i f =
  case index i s of
     Just old -> replace (f old) i s
     Nothing -> s
