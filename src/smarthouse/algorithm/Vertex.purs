module SmartHouse.Algorithm.Vertex where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.Line (Line, line)
import Math.LineSeg (LineSeg, lineVec)
import Three.Math.Vector (class Vector, Vector3, normal, vecX, vecY, (<**>), (<+>))


type Ray = Line Vector3

ray :: Vector3 -> Vector3 -> Ray
ray = line
    
newtype Vertex = Vertex {
    index     :: Int,
    position  :: Vector3,
    leftEdge  :: LineSeg Vector3,
    rightEdge :: LineSeg Vector3,
    isReflex  :: Boolean,
    bisector  :: Ray
    }

derive instance newtypeVertex :: Newtype Vertex _
derive instance genericVertex :: Generic Vertex _
derive instance eqVertex :: Eq Vertex
instance showVertex :: Show Vertex where
    show = genericShow

_leftEdge :: forall t a r. Newtype t { leftEdge :: a | r } => Lens' t a
_leftEdge = _Newtype <<< prop (SProxy :: SProxy "leftEdge")

_rightEdge :: forall t a r. Newtype t { rightEdge :: a | r } => Lens' t a
_rightEdge = _Newtype <<< prop (SProxy :: SProxy "rightEdge")

_isReflex :: forall t a r. Newtype t { isReflex :: a | r } => Lens' t a
_isReflex = _Newtype <<< prop (SProxy :: SProxy "isReflex")

_bisector :: forall t a r. Newtype t { bisector :: a | r } => Lens' t a
_bisector = _Newtype <<< prop (SProxy :: SProxy "bisector")

_cross :: forall v. Vector v => v -> v -> Number
_cross v1 v2 = vecX v1 * vecY v2 - vecX v2 * vecY v1

-- create a Vectex from a point and edges it connects to
vertexFrom :: Int -> Vector3 -> LineSeg Vector3 -> LineSeg Vector3 -> Vertex
vertexFrom idx p leftEdge rightEdge =
    let lv       = normal $ lineVec leftEdge <**> (-1.0)
        rv       = normal $ lineVec rightEdge
        isReflex = _cross lv rv < 0.0
        dir      = (lv <+> rv) <**> (if isReflex then -1.0 else 1.0)
    in Vertex {
        index     : idx,
        position  : p,
        leftEdge  : leftEdge,
        rightEdge : rightEdge,
        isReflex  : isReflex,
        bisector  : ray p dir
    }
