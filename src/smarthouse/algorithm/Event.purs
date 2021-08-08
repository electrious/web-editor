module SmartHouse.Algorithm.Event where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3, vecZ)
import Type.Proxy (Proxy(..))

-- event to merge two consecutive bisector and shrink an edge to zero
newtype EdgeE = EdgeE {
    edge         :: Edge,
    intersection :: Vector3,
    vertexA      :: Vertex,
    vertexB      :: Vertex
}

derive instance newtypeEdgeE :: Newtype EdgeE _
derive instance genericEdgeE :: Generic EdgeE _
instance showEdgeE :: Show EdgeE where
    show = genericShow

_intersection :: forall t a r. Newtype t { intersection :: a | r } => Lens' t a
_intersection = _Newtype <<< prop (Proxy :: Proxy "intersection")

_vertexA :: forall t a r. Newtype t { vertexA :: a | r } => Lens' t a
_vertexA = _Newtype <<< prop (Proxy :: Proxy "vertexA")

_vertexB :: forall t a r. Newtype t { vertexB :: a | r } => Lens' t a
_vertexB = _Newtype <<< prop (Proxy :: Proxy "vertexB")


-- event to merge three consecutive bisectors at one intersection point
newtype EdgesE = EdgesE {
    edge         :: Edge,
    intersection :: Vector3,
    vertexA      :: Vertex,
    vertexB      :: Vertex,
    vertexC      :: Vertex
    }

derive instance newtypeEdgesE :: Newtype EdgesE _
derive instance genericEdgesE :: Generic EdgesE _
instance showEdgesE :: Show EdgesE where
    show = genericShow

_vertexC :: forall t a r. Newtype t { vertexC :: a | r } => Lens' t a
_vertexC = _Newtype <<< prop (Proxy :: Proxy "vertexC")


newtype SplitE = SplitE {
    edge         :: Edge,
    intersection :: Vector3,
    vertex       :: Vertex,
    oppositeEdge :: Edge
    }

derive instance newtypeSplitE :: Newtype SplitE _
derive instance genericSplitE :: Generic SplitE _
instance showSplitE :: Show SplitE where
    show = genericShow


_oppositeEdge :: forall t a r. Newtype t { oppositeEdge :: a | r } => Lens' t a
_oppositeEdge = _Newtype <<< prop (Proxy :: Proxy "oppositeEdge")


data PointEvent = EdgeEvent EdgeE
                | EdgesEvent EdgesE
                | SplitEvent SplitE

derive instance genericPointEvent :: Generic PointEvent _
instance showPointEvent :: Show PointEvent where
    show = genericShow


edgeE :: Edge -> Vector3 -> Vertex -> Vertex -> PointEvent
edgeE e p va vb = EdgeEvent $ EdgeE {
    edge         : e,
    intersection : p,
    vertexA      : va,
    vertexB      : vb
    }

edgesE :: Edge -> Vector3 -> Vertex -> Vertex -> Vertex -> PointEvent
edgesE e p va vb vc = EdgesEvent $ EdgesE {
    edge         : e,
    intersection : p,
    vertexA      : va,
    vertexB      : vb,
    vertexC      : vc
    }

splitE :: Vector3 -> Vertex -> Edge -> PointEvent
splitE p v e = SplitEvent $ SplitE {
    edge         : e,
    intersection : p,
    vertex       : v,
    oppositeEdge : e
    }

intersectionPoint :: PointEvent -> Vector3
intersectionPoint (EdgeEvent e)  = e ^. _intersection
intersectionPoint (EdgesEvent e) = e ^. _intersection
intersectionPoint (SplitEvent e) = e ^. _intersection

distance :: PointEvent -> Number
distance = vecZ <<< intersectionPoint
