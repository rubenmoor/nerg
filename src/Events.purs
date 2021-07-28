module Events
  ( onEvent
  , onEventE
  , gridBeat
  , gamePeriod
  , gameSpeed
  ) where

import Prelude

import Effect (Effect)
import Signal (Signal, runSignal)
import Signal.Time (every)

-- game speed in steps per second
gameSpeed :: Number
gameSpeed = 4.0

-- time per game cycle in miliseconds
gamePeriod :: Number
gamePeriod = 1000.0 / gameSpeed

gridBeat :: Signal Number
gridBeat = every gamePeriod

onEvent :: forall a. Signal a -> (a -> Effect Unit) -> Effect Unit
onEvent s f = runSignal $ f <$> s

onEventE :: forall a. Effect (Signal a) -> (a -> Effect Unit) -> Effect Unit
onEventE m f = m >>= flip onEvent f
