module SmartHouse.Algorithm.Vertex where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _position)
import Effect (Effect)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _line)
import SmartHouse.Algorithm.Ray (Ray)
import SmartHouse.Algorithm.VertInfo (VertInfo, _bisector, _isReflex, _usable, vertInfoFrom)
import Three.Math.Vector (Vector3)

    
newtype Vertex = Vertex {
    id        :: UUID,
    position  :: Vector3,
    leftEdge  :: Edge,
    rightEdge :: Edge,
    isReflex  :: Boolean,
    bisector  :: Ray,
    lavId     :: UUID,
    usable    :: Boolean  -- whether the vertex will be able to have a next event
    }

derive instance newtypeVertex :: Newtype Vertex _
derive instance genericVertex :: Generic Vertex _
instance eqVertex :: Eq Vertex where
    eq v1 v2 = v1 ^. idLens == v2 ^. idLens
instance showVertex :: Show Vertex where
    show = genericShow
instance hasUUID :: HasUUID Vertex where
    idLens = _id

_leftEdge :: forall t a r. Newtype t { leftEdge :: a | r } => Lens' t a
_leftEdge = _Newtype <<< prop (SProxy :: SProxy "leftEdge")

_rightEdge :: forall t a r. Newtype t { rightEdge :: a | r } => Lens' t a
_rightEdge = _Newtype <<< prop (SProxy :: SProxy "rightEdge")

_lavId :: forall t a r. Newtype t { lavId :: a | r } => Lens' t a
_lavId = _Newtype <<< prop (SProxy :: SProxy "lavId")


-- create a Vectex from a point and edges it connects to
vertexFrom :: UUID -> Vector3 -> Edge -> Edge -> Maybe Vector3 -> Maybe Vector3 -> Effect Vertex
vertexFrom lavId p leftEdge rightEdge vecL vecR = vertexFromVertInfo lavId leftEdge rightEdge vi
    where vi = vertInfoFrom p (leftEdge ^. _line) (rightEdge ^. _line) vecL vecR

-- create vertex from edges and VertInfo data
vertexFromVertInfo :: UUID -> Edge -> Edge -> VertInfo -> Effect Vertex
vertexFromVertInfo lavId leftEdge rightEdge vi = do
    i <- genUUID
    pure $ Vertex {
        id        : i,
        position  : vi ^. _position,
        leftEdge  : leftEdge,
        rightEdge : rightEdge,
        isReflex  : vi ^. _isReflex,
        bisector  : vi ^. _bisector,
        lavId     : lavId,
        usable    : vi ^. _usable
    }
