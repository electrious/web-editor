module SmartHouse.Algorithm.Vertex where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id)
import Effect (Effect)
import Math.Line (Line, line)
import Math.LineSeg (LineSeg, lineVec)
import Model.UUID (class HasUUID, idLens)
import Three.Math.Vector (class Vector, Vector3, normal, vecX, vecY, (<**>), (<+>))


type Ray = Line Vector3

ray :: Vector3 -> Vector3 -> Ray
ray = line
    
newtype Vertex = Vertex {
    id        :: UUID,
    position  :: Vector3,
    leftEdge  :: LineSeg Vector3,
    rightEdge :: LineSeg Vector3,
    isReflex  :: Boolean,
    bisector  :: Ray,
    lavId     :: UUID
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

_isReflex :: forall t a r. Newtype t { isReflex :: a | r } => Lens' t a
_isReflex = _Newtype <<< prop (SProxy :: SProxy "isReflex")

_bisector :: forall t a r. Newtype t { bisector :: a | r } => Lens' t a
_bisector = _Newtype <<< prop (SProxy :: SProxy "bisector")

_lavId :: forall t a r. Newtype t { lavId :: a | r } => Lens' t a
_lavId = _Newtype <<< prop (SProxy :: SProxy "lavId")

_cross :: forall v. Vector v => v -> v -> Number
_cross v1 v2 = vecX v1 * vecY v2 - vecX v2 * vecY v1

-- create a Vectex from a point and edges it connects to
vertexFrom :: UUID -> Vector3 -> LineSeg Vector3 -> LineSeg Vector3 -> Effect Vertex
vertexFrom lavId p leftEdge rightEdge = do
    i <- genUUID
    let lv       = normal $ lineVec leftEdge <**> (-1.0)
        rv       = normal $ lineVec rightEdge
        isReflex = _cross lv rv < 0.0
        dir      = (lv <+> rv) <**> (if isReflex then -1.0 else 1.0)
    pure $ Vertex {
        id        : i,
        position  : p,
        leftEdge  : leftEdge,
        rightEdge : rightEdge,
        isReflex  : isReflex,
        bisector  : ray p dir,
        lavId     : lavId
    }
