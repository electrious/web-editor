module Model.Polygon where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Three.Math.Vector (Vector2)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _
derive instance eqPolygon :: Eq Polygon

_polyVerts :: Lens' Polygon (Array Vector2)
_polyVerts = _Newtype
