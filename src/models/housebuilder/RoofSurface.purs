module Model.HouseBuilder.RoofSurface where

import Prelude

import Data.Default (class Default, def)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_polygon)
import Effect (Effect)
import Model.HouseBuilder.HousePoint (HousePoint, HousePointType(..), newHousePoint, ridgePoints)
import Model.Polygon (Polygon, _polyVerts, polygonAround)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)

newtype RoofSurface = RoofSurface {
    polygon     :: Polygon HousePoint,
    ridgePoints :: List HousePoint
    }

derive instance newtypeRoofSurface :: Newtype RoofSurface _
instance defaultRoofSurface :: Default RoofSurface where
    def = RoofSurface {
        polygon     : def,
        ridgePoints : Nil
        }

_ridgePoints :: forall t a r. Newtype t { ridgePoints :: a | r } => Lens' t a
_ridgePoints = _Newtype <<< prop (SProxy :: SProxy "ridgePoints")

allPoints :: RoofSurface -> Array HousePoint
allPoints s = s ^. _polygon <<< _polyVerts

newSurface :: Polygon HousePoint -> RoofSurface
newSurface poly = def # _polygon     .~ poly
                      # _ridgePoints .~ ridgePoints (fromFoldable $ poly ^. _polyVerts)

-- | create a new RoofSurface around a point
surfaceAround :: Vector3 -> Effect RoofSurface
surfaceAround p = newSurface <$> traverse mkHP (polygonAround $ toVec2 p)
    where mkHP v = newHousePoint RidgePoint (mkVec3 (vecX v) (vecY v) 0.01)
