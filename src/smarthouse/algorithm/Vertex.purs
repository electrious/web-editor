module SmartHouse.Algorithm.Vertex where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Three.Math.Vector (Vector2, Vector3)

newtype Ray = Ray {
    start     :: Vector2,
    direction :: Vector2
    }

derive instance newtypeRay :: Newtype Ray _
derive instance genericRay :: Generic Ray _
derive instance eqRay :: Eq Ray
instance showRay :: Show Ray where
    show = genericShow

_direction :: forall t a r. Newtype t { direction :: a | r } => Lens' t a
_direction = _Newtype <<< prop (SProxy :: SProxy "direction")

newtype Vertex = Vertex {
    index    :: Int,
    position :: Vector3,
    isReflex :: Boolean,
    bisector :: Ray
    }

derive instance newtypeVertex :: Newtype Vertex _
derive instance genericVertex :: Generic Vertex _
derive instance eqVertex :: Eq Vertex
instance showVertex :: Show Vertex where
    show = genericShow

_isReflex :: forall t a r. Newtype t { isReflex :: a | r } => Lens' t a
_isReflex = _Newtype <<< prop (SProxy :: SProxy "isReflex")

_bisector :: forall t a r. Newtype t { bisector :: a | r } => Lens' t a
_bisector = _Newtype <<< prop (SProxy :: SProxy "bisector")
