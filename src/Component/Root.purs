module Component.Root where

import Component.CanvasGrid (canvasGrid, initGrid)
import Component.CanvasGrid as CanvasGrid
import Control.Bind (pure)
import Data.Const (Const)
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Effect.Aff (Aff)
import Grid.Internal (randomGrid)
import Halogen (Component, ParentHTML, ParentDSL, parentComponent)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1)
import Halogen.HTML (HTML, div_, slot')

type State = Unit
type Input = Unit
data Query a = Query a
type ChildSlot =
     Unit
  \/ Void
type ChildQuery =
       CanvasGrid.Query
  <\/> Const Void
type Message = Void

root :: Component HTML Query Input Message Aff
root = parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }

render :: State -> ParentHTML Query ChildQuery ChildSlot Aff
render state = div_
  [ slot' cp1 unit canvasGrid (initGrid randomGrid) absurd
  ]

eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Message Aff
eval (Query next) = pure next
