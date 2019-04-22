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
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Graphics.Drawing as Drawing
import Grid (nextState)
import Grid.Draw (drawUpdate, redrawGrid)
import Grid.Internal (Grid)
import Halogen (Component, ComponentDSL, ComponentHTML, action, lifecycleComponent, liftEffect)
import Halogen.HTML (HTML, button, div_, text)
import Halogen.HTML as HTML
import Halogen.HTML.Events as Events
import Halogen.HTML.Properties as P
import Partial.Unsafe (unsafePartial)

elemId :: String
elemId = "canvas"

type State =
  { _context :: Maybe Context2D
  , _grid :: Grid
  }

context :: Lens' State (Maybe Context2D)
context = lens _._context (\r c -> r { _context = c })

grid :: Lens' State Grid
grid = lens _._grid (\r g -> r { _grid = g })

data Query a =
    Initialize a
  | NextStep a

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
      [ P.id_ elemId
      , P.width 640
      , P.height 640
      ]
    ]
  , div_
    [ button
      [ Events.onClick $ Events.input_ NextStep
      ]
      [ text "next"
      ]
    ]
  ]

eval :: Query ~> ComponentDSL State Query Message Aff
eval = case _ of
  Initialize next -> do
    ctx <- liftEffect $ do
      mCnv <- getCanvasElementById elemId
      let cnv = unsafePartial (fromJust mCnv)
      getContext2D cnv
    g <- use grid
    liftEffect $ Drawing.render ctx $ redrawGrid g
    context .= Just ctx
    pure next
  NextStep next -> do
    oldGrid <- use grid
    let Tuple update newGrid = nextState oldGrid
    mCtx <- use context
    let ctx = unsafePartial $ fromJust mCtx
    liftEffect $ Drawing.render ctx $ drawUpdate update
    grid .= newGrid
    pure next

initGrid :: Grid -> State
initGrid g =
  { _context: Nothing
  , _grid: g
  }
