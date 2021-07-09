module SmartHouse.Algorithm.VertNode where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_height, _id, _position)
import Effect (Effect)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3)

-- A node representing a vertex node in the final subtrees.
newtype VertNode = VertNode {
    id       :: UUID,
    position :: Vector3,
    height   :: Number
}

derive instance newtypeVertNode :: Newtype VertNode _
derive instance genericVertNode :: Generic VertNode _
instance showVertNode :: Show VertNode where
    show = genericShow
instance eqVertNode :: Eq VertNode where
    eq v1 v2 = v1 ^. idLens == v2 ^. idLens
instance hasUUIDVertNode :: HasUUID VertNode where
    idLens = _id

vertNodeFromVertex :: Vertex -> VertNode
vertNodeFromVertex v = VertNode {
    id       : v ^. idLens,
    position : v ^. _position,
    height   : v ^. _height
}

mkVertNode :: Vector3 -> Number -> Effect VertNode
mkVertNode p h = do
    i <- genUUID
    pure $ VertNode {
        id       : i,
        position : p,
        height   : h
    }