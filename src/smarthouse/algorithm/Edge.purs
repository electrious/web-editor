module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.Line (Line)
import SmartHouse.Algorithm.Vertex (Ray)
import Three.Math.Vector (Vector3)

newtype Edge = Edge {
    line          :: Line Vector3,
    leftBisector  :: Ray,
    rightBisector :: Ray
    }

derive instance newtypeEdge :: Newtype Edge _
derive instance genericEdge :: Generic Edge _
instance showEdge :: Show Edge where
    show = genericShow

_line :: forall t a r. Newtype t { line :: a | r } => Lens' t a
_line = _Newtype <<< prop (SProxy :: SProxy "line")

_leftBisector :: forall t a r. Newtype t { leftBisector :: a | r } => Lens' t a
_leftBisector = _Newtype <<< prop (SProxy :: SProxy "leftBisector")

_rightBisector :: forall t a r. Newtype t { rightBisector :: a | r } => Lens' t a
_rightBisector = _Newtype <<< prop (SProxy :: SProxy "rightBisector")
