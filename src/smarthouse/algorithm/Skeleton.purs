module SmartHouse.Algorithm.Skeleton where

import Prelude

import Data.Filterable (filter)
import Data.Lens ((^.))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_position)
import Math (abs)
import Math.LineSeg (intersection, lineVec)
import Math.Utils (approxSame)
import SmartHouse.Algorithm.Edge (Edge, _line)
import SmartHouse.Algorithm.Event (PointEvent)
import SmartHouse.Algorithm.LAV (SLAV, _edges)
import SmartHouse.Algorithm.Vertex (Vertex, _leftEdge, _rightEdge)
import Three.Math.Vector (Vector3, normal, (<.>))

-- next event for a reflex vertex
nextEvtForReflex :: SLAV -> Vertex -> List PointEvent
nextEvtForReflex slav v =
    let lEdge = v ^. _leftEdge
        rEdge = v ^. _rightEdge
        
        -- check if an edge is the left/right edge of the vertex v
        notNearby e = not $ e ^. _line == lEdge || e ^. _line == rEdge

        -- a potential b is at the intersection of between our own bisector and the bisector of the
        -- angle between the tested edge and any one of our own edges.

        -- we choose the "less parallel" edge (in order to exclude a potentially parallel edge)
        normEdgeVec = normal <<< lineVec
        
        intersectP :: Edge -> Maybe Vector3
        intersectP e = let eVec      = normEdgeVec $ e ^. _line
                           leftdot   = abs $ normEdgeVec lEdge <.> eVec
                           rightdot  = abs $ normEdgeVec rEdge <.> eVec
                           selfEdge  = if leftdot < rightdot then lEdge else rEdge
                           otherEdge = if leftdot < rightdot then rEdge else lEdge
                       in intersection selfEdge otherEdge

        notV i = not $ approxSame i $ v ^. _position

        -- find a valid intersection point
        validIntersectP :: Edge -> Maybe (Tuple Edge Vector3)
        validIntersectP e = intersectP e >>= (\i -> if notV i then Just $ Tuple e i else Nothing)

        -- locate candidate b
        

        edges = filter notNearby (slav ^. _edges)

    in Nil
