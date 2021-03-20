module SmartHouse.Algorithm.Event where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3)


newtype EdgeE = EdgeE {
    distance     :: Number,
    intersection :: Vector3,
    vertexA      :: Vertex,
    vertexB      :: Vertex
}

derive instance newtypeEdgeE :: Newtype EdgeE _
derive instance genericEdgeE :: Generic EdgeE _
instance showEdgeE :: Show EdgeE where
    show = genericShow

_intersection :: forall t a r. Newtype t { intersection :: a | r } => Lens' t a
_intersection = _Newtype <<< prop (SProxy :: SProxy "intersection")

_vertexA :: forall t a r. Newtype t { vertexA :: a | r } => Lens' t a
_vertexA = _Newtype <<< prop (SProxy :: SProxy "vertexA")

_vertexB :: forall t a r. Newtype t { vertexB :: a | r } => Lens' t a
_vertexB = _Newtype <<< prop (SProxy :: SProxy "vertexB")


newtype SplitE = SplitE {
    distance     :: Number,
    intersection :: Vector3,
    vertex       :: Vertex,
    oppositeEdge :: Edge
    }

derive instance newtypeSplitE :: Newtype SplitE _
derive instance genericSplitE :: Generic SplitE _
instance showSplitE :: Show SplitE where
    show = genericShow


_oppositeEdge :: forall t a r. Newtype t { oppositeEdge :: a | r } => Lens' t a
_oppositeEdge = _Newtype <<< prop (SProxy :: SProxy "oppositeEdge")


data PointEvent = EdgeEvent EdgeE
                | SplitEvent SplitE

derive instance genericPointEvent :: Generic PointEvent _
instance showPointEvent :: Show PointEvent where
    show = genericShow


edgeE :: Number -> Vector3 -> Vertex -> Vertex -> PointEvent
edgeE dist p va vb = EdgeEvent $ EdgeE {
    distance     : dist,
    intersection : p,
    vertexA      : va,
    vertexB      : vb
    }

splitE :: Number -> Vector3 -> Vertex -> Edge -> PointEvent
splitE dist p v e = SplitEvent $ SplitE {
    distance     : dist,
    intersection : p,
    vertex       : v,
    oppositeEdge : e
    }

intersectionPoint :: PointEvent -> Vector3
intersectionPoint (EdgeEvent e)  = e ^. _intersection
intersectionPoint (SplitEvent e) = e ^. _intersection
