module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Editor.Common.Lenses (_id)
import Math.LineSeg (LineSeg)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.EdgeInfo (EdgeInfo, _line)
import SmartHouse.Algorithm.Ray (Ray)
import SmartHouse.Algorithm.VertNode (VertNode)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

newtype Edge = Edge {
    id            :: UUID,
    line          :: EdgeInfo,
    leftVertex    :: VertNode,
    rightVertex   :: VertNode,
    leftBisector  :: Ray,
    rightBisector :: Ray,

    normal        :: Vector3  -- outward normal vector for an edge
    }

derive instance newtypeEdge :: Newtype Edge _
derive instance genericEdge :: Generic Edge _
instance eqEdge :: Eq Edge where
    eq e1 e2 = e1 ^. idLens == e2 ^. idLens
instance ordEdge :: Ord Edge where
    compare = comparing (view idLens)
instance showEdge :: Show Edge where
    show = genericShow
instance hasUUIDEdge :: HasUUID Edge where
    idLens = _id

_leftVertex :: forall t a r. Newtype t { leftVertex :: a | r } => Lens' t a
_leftVertex = _Newtype <<< prop (Proxy :: Proxy "leftVertex")

_rightVertex :: forall t a r. Newtype t { rightVertex :: a | r } => Lens' t a
_rightVertex = _Newtype <<< prop (Proxy :: Proxy "rightVertex")

_leftBisector :: forall t a r. Newtype t { leftBisector :: a | r } => Lens' t a
_leftBisector = _Newtype <<< prop (Proxy :: Proxy "leftBisector")

_rightBisector :: forall t a r. Newtype t { rightBisector :: a | r } => Lens' t a
_rightBisector = _Newtype <<< prop (Proxy :: Proxy "rightBisector")

_lineEdge :: Lens' Edge (LineSeg Vector3)
_lineEdge = _line <<< _line
