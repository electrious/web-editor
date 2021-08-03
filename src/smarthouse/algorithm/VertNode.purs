module SmartHouse.Algorithm.VertNode where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (%~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_edge, _height, _id, _position, _slope)
import Effect (Effect)
import Math.Angle (Angle, degreeVal, tan)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.EdgeInfo (_line)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY)

-- A node representing a vertex node in the final subtrees.
newtype VertNode = VertNode {
    id       :: UUID,
    position :: Vector3,
    height   :: Number
}

derive instance Newtype VertNode _
derive instance Generic VertNode _
instance Show VertNode where
    show = genericShow
instance Eq VertNode where
    eq v1 v2 = v1 ^. idLens == v2 ^. idLens
instance HasUUID VertNode where
    idLens = _id
instance Default VertNode where
    def = VertNode {
        id       : emptyUUID,
        position : def,
        height   : 0.0
    }


scaleFactor :: Angle -> Number
scaleFactor a | degreeVal a < 90.0 = tan a
              | otherwise          = 0.0

setZ :: Number -> Vector3 -> Vector3
setZ z v = mkVec3 (vecX v) (vecY v) z

-- project a subtree node source's Z to 3D value based on slope and distance to corresponding edge
projNodeTo3D :: Angle -> VertNode -> VertNode
projNodeTo3D slope v = v # _position %~ setZ (v ^. _height * s)
    where s = scaleFactor slope

vertNodeFromVertex :: Vertex -> VertNode
vertNodeFromVertex v =
    let vn = VertNode {
                id       : v ^. idLens,
                position : v ^. _position,
                height   : v ^. _height
            }
        s = view (_line <<< _slope) <$> v ^. _edge
    in case s of
        Nothing -> vn
        Just slope -> projNodeTo3D slope vn

mkVertNode :: Vector3 -> Edge -> Number -> Effect VertNode
mkVertNode p e h = do
    i <- genUUID
    pure $ projNodeTo3D (e ^. _line <<< _slope) $ VertNode {
        id       : i,
        position : p,
        height   : h
    }