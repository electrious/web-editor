module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _position)
import Effect (Effect)
import Math.LineSeg (LineSeg, mkLineSeg)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3)

newtype Edge = Edge {
    id          :: UUID,
    line        :: LineSeg Vector3,
    leftVertex  :: Vertex,
    rightVertex :: Vertex
    }

derive instance newtypeEdge :: Newtype Edge _
derive instance genericEdge :: Generic Edge _
instance eqEdge :: Eq Edge where
    eq e1 e2 = e1 ^. idLens == e2 ^. idLens
instance showEdge :: Show Edge where
    show = genericShow
instance hasUUIDEdge :: HasUUID Edge where
    idLens = _id

_line :: forall t a r. Newtype t { line :: a | r } => Lens' t a
_line = _Newtype <<< prop (SProxy :: SProxy "line")

_leftVertex :: forall t a r. Newtype t { leftVertex :: a | r } => Lens' t a
_leftVertex = _Newtype <<< prop (SProxy :: SProxy "leftVertex")

_rightVertex :: forall t a r. Newtype t { rightVertex :: a | r } => Lens' t a
_rightVertex = _Newtype <<< prop (SProxy :: SProxy "rightVertex")

edge :: Vertex -> Vertex -> Effect Edge
edge lv rv = do
    i <- genUUID
    pure $ Edge {
        id          : i,
        line        : mkLineSeg (lv ^. _position) (rv ^. _position),
        leftVertex  : lv,
        rightVertex : rv
        }
