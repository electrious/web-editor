module SmartHouse.Algorithm.VertNode where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_id, _position)
import Effect (Effect)
import Model.UUID (class HasUUID, idLens)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)

-- A node representing a vertex node in the final subtrees.
newtype VertNode = VertNode {
    id       :: UUID,
    position :: Vector3
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
        position : def
    }

setZ :: Number -> Vector3 -> Vector3
setZ z v = mkVec3 (vecX v) (vecY v) z

height :: VertNode -> Number
height node = vecZ $ node ^. _position

mkVertNode :: Vector3 -> Effect VertNode
mkVertNode p = do
    i <- genUUID
    pure $ VertNode {
        id       : i,
        position : p
    }