module SmartHouse.Algorithm.VertNode where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens ((%~), (^.))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _position)
import Effect (Effect)
import Model.UUID (class HasUUID, idLens)
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
instance Ord VertNode where
    compare v1 v2 = compare (v1 ^. idLens) (v2 ^. idLens)
instance HasUUID VertNode where
    idLens = _id
instance Default VertNode where
    def = VertNode {
        id       : emptyUUID,
        position : def,
        height   : 0.0
    }

setZ :: Number -> Vector3 -> Vector3
setZ z v = mkVec3 (vecX v) (vecY v) z

-- project a subtree node source's Z to 3D value based on height of the VertNode
projNodeTo3D :: VertNode -> VertNode
projNodeTo3D v = v # _position %~ setZ (v ^. _height)

mkVertNode :: Vector3 -> Number -> Effect VertNode
mkVertNode p h = do
    i <- genUUID
    pure $ projNodeTo3D $ VertNode {
        id       : i,
        position : p,
        height   : h
    }