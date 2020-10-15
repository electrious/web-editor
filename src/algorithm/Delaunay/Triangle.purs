module Algorithm.Delaunay.Triangle where

import Prelude

import Algorithm.Delaunay.Vertex (class Vertex, vertX, vertY)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Math (abs)

foreign import epsilon :: Number


newtype Edge v = Edge {
    vertex1 :: v,
    vertex2 :: v
}

derive instance newtypeEdge :: Newtype (Edge v) _
derive instance eqEdge :: Eq v => Eq (Edge v)

mkEdge :: forall v. v -> v -> Edge v
mkEdge v1 v2 = Edge { vertex1 : v1, vertex2 : v2 }

newtype Triangle v = Triangle {
    vertex1 :: v,
    vertex2 :: v,
    vertex3 :: v,

    -- center of the circumcircle of the triangle
    x       :: Number,
    y       :: Number,

    -- squared radius of the circumcircle
    rsqr    :: Number
}

derive instance newtypeTriangle :: Newtype (Triangle v) _

_vertex1 :: forall t a r. Newtype t { vertex1 :: a | r } => Lens' t a
_vertex1 = _Newtype <<< prop (SProxy :: SProxy "vertex1")

_vertex2 :: forall t a r. Newtype t { vertex2 :: a | r } => Lens' t a
_vertex2 = _Newtype <<< prop (SProxy :: SProxy "vertex2")

_vertex3 :: forall t a r. Newtype t { vertex3 :: a | r } => Lens' t a
_vertex3 = _Newtype <<< prop (SProxy :: SProxy "vertex3")

_rsqr :: forall t a r. Newtype t { rsqr :: a | r } => Lens' t a
_rsqr = _Newtype <<< prop (SProxy :: SProxy "rsqr")

triangleEdges :: forall v. Triangle v -> List (Edge v)
triangleEdges tri = e1 : e2 : e3 : Nil
    where v1 = tri ^. _vertex1
          v2 = tri ^. _vertex2
          v3 = tri ^. _vertex3
          e1 = mkEdge v1 v2
          e2 = mkEdge v2 v3
          e3 = mkEdge v3 v1

-- create a triangle from three vertices
mkTriangle :: forall v. Vertex v => v -> v -> v -> Triangle v
mkTriangle i j k = Triangle { vertex1: i,
                              vertex2: j,
                              vertex3: k,
                              x      : xc,
                              y      : yc,
                              rsqr   : rsqr
                             }
    where x1 = vertX i
          y1 = vertY i
          x2 = vertX j
          y2 = vertY j
          x3 = vertX k
          y3 = vertY k

          fabsy1y2 = abs $ y1 - y2
          fabsy2y3 = abs $ y2 - y3

          Tuple xc yc = if fabsy1y2 < epsilon
                        then let m2  = -((x3 - x2) / (y3 - y2))
                                 mx2 = (x2 + x3) / 2.0
                                 my2 = (y2 + y3) / 2.0
                                 xc  = (x2 + x1) / 2.0
                                 yc  = m2 * (xc - mx2) + my2
                             in Tuple xc yc
                        else if fabsy2y3 < epsilon
                            then let m1  = -((x2 - x1) / (y2 - y1))
                                     mx1 = (x1 + x2) / 2.0
                                     my1 = (y1 + y2) / 2.0
                                     xc  = (x3 + x2) / 2.0
                                     yc  = m1 * (xc - mx1) + my1
                                 in Tuple xc yc
                            else let m1 = -((x2 - x1) / (y2 - y1))
                                     m2 = -((x3 - x2) / (y3 - y2))
                                     mx1 = (x1 + x2) / 2.0
                                     mx2 = (x2 + x3) / 2.0
                                     my1 = (y1 + y2) / 2.0
                                     my2 = (y2 + y3) / 2.0
                                     xc = (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2)
                                     yc = if fabsy1y2 > fabsy2y3
                                          then m1 * (xc - mx1) + my1
                                          else m2 * (xc - mx2) + my2
                                 in Tuple xc yc
          
          dx = x2 - xc
          dy = y2 - yc
          rsqr = dx * dx + dy * dy