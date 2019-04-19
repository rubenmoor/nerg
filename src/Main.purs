module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, render)
import Grid.Internal (randomGrid)
import Grid.Render (redrawGrid)

main :: Effect Unit
main = do
    getCanvasElementById "canvas" >>= case _ of
      Nothing -> Console.log ("canvas not found.")
      Just canvas -> do
        ctx <- getContext2D canvas
        render ctx drawing

drawing :: Drawing
drawing = redrawGrid randomGrid
