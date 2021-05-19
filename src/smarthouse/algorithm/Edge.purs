module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_position)
import Math.LineSeg (LineSeg, mkLineSeg)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3)

newtype Edge = Edge {
    line        :: LineSeg Vector3,
    leftVertex  :: Vertex,
    rightVertex :: Vertex
    }

derive instance newtypeEdge :: Newtype Edge _
derive instance genericEdge :: Generic Edge _
instance showEdge :: Show Edge where
    show = genericShow

_line :: forall t a r. Newtype t { line :: a | r } => Lens' t a
_line = _Newtype <<< prop (SProxy :: SProxy "line")

_leftVertex :: forall t a r. Newtype t { leftVertex :: a | r } => Lens' t a
_leftVertex = _Newtype <<< prop (SProxy :: SProxy "leftVertex")

_rightVertex :: forall t a r. Newtype t { rightVertex :: a | r } => Lens' t a
_rightVertex = _Newtype <<< prop (SProxy :: SProxy "rightVertex")

edge :: Vertex -> Vertex -> Edge
edge lv rv = Edge {
    line        : mkLineSeg (lv ^. _position) (rv ^. _position),
    leftVertex  : lv,
    rightVertex : rv
    }
