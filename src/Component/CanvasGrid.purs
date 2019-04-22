module Component.CanvasGrid where

import Control.Bind (discard)
import Control.Monad (pure, bind)
import Data.Function (const, identity, ($))
import Data.Lens (Lens', lens, use, (.=))
import Data.Maybe (Maybe(..), fromJust)
import Data.NaturalTransformation (type (~>))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Effect.Aff (Aff)
import Effect.Timer (clearInterval, setInterval)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, translate)
import Graphics.Drawing as Drawing
import Grid (nextState)
import Grid.Draw (drawUpdate, redrawGrid)
import Grid.Internal (Grid, gridHeight, gridWidth)
import Halogen (Component, ComponentDSL, ComponentHTML, SubscribeStatus(..), action, lifecycleComponent, liftEffect, request, subscribe)
import Halogen.HTML (HTML, button, div_, text)
import Halogen.HTML as HTML
import Halogen.HTML.Events as Events
import Halogen.HTML.Properties as P
import Halogen.Query.EventSource (eventSource_')
import Partial.Unsafe (unsafePartial)

canvasId :: String
canvasId = "canvas"

type State =
  { _context :: Maybe Context2D
  , _grid :: Grid
  , _autoRun :: Boolean
  }

-- lens definitions

context :: Lens' State (Maybe Context2D)
context = lens _._context (\r c -> r { _context = c })

grid :: Lens' State Grid
grid = lens _._grid (\r g -> r { _grid = g })

autoRun :: Lens' State Boolean
autoRun = lens _._autoRun (\r b -> r { _autoRun = b })

data Query a =
    Initialize a
  | ToggleAutoRun a
  | NextStep (SubscribeStatus -> a)

type Input = State

type Message = Void

canvasGrid :: Component HTML Query Input Message Aff
canvasGrid = lifecycleComponent
  { initialState: identity
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ action Initialize
  , finalizer: Nothing
  }

render :: State -> ComponentHTML Query
render state = div_
  [ div_
    [ HTML.canvas
      [ P.id_ canvasId
      , P.width gridWidth
      , P.height gridHeight
      ]
    ]
  , div_
    [ button
      [ Events.onClick $ Events.input_ ToggleAutoRun
      ]
      [ text $ if state._autoRun then "stop" else "run"
      ]
    ]
  ]

eval :: Query ~> ComponentDSL State Query Message Aff
eval = case _ of
  Initialize next -> do
    ctx <- liftEffect $ do
      mCnv <- getCanvasElementById canvasId
      let cnv = unsafePartial (fromJust mCnv)
      getContext2D cnv
    g <- use grid
    liftEffect $ do
      translate ctx { translateX: 0.05, translateY: 0.05 }
      Drawing.render ctx $ redrawGrid g
    context .= Just ctx
    pure next
  ToggleAutoRun next -> do
    running <- use autoRun
    if running
       then autoRun .= false
       else do
         autoRun .= true
         let attach callback = do
               intId <- setInterval 100 callback
               pure $ clearInterval intId
         subscribe $ eventSource_' attach (request NextStep)
    pure next
  NextStep reply -> do
    running <- use autoRun
    if running
       then do
         oldGrid <- use grid
         let Tuple update newGrid = nextState oldGrid
         mCtx <- use context
         let ctx = unsafePartial $ fromJust mCtx
         liftEffect $ Drawing.render ctx $ drawUpdate update
         grid .= newGrid
         pure $ reply Listening
       else pure $ reply Done

initGrid :: Grid -> State
initGrid g =
  { _context: Nothing
  , _grid: g
  , _autoRun: false
  }
