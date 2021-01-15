module Model.HouseBuilder.RoofSurface where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_id, _polygon)
import Effect (Effect)
import Model.HouseBuilder.HousePoint (HousePoint, HousePointType(..), newHousePoint, ridgePoints)
import Model.Polygon (Polygon, _polyVerts, polygonAround)
import Model.UUID (class HasUUID)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)

newtype RoofSurface = RoofSurface {
    id          :: UUID,
    polygon     :: Polygon HousePoint,
    ridgePoints :: List HousePoint
    }

derive instance genericRoofSurface :: Generic RoofSurface _
derive instance newtypeRoofSurface :: Newtype RoofSurface _
instance showRoofSurface :: Show RoofSurface where
    show = genericShow
instance defaultRoofSurface :: Default RoofSurface where
    def = RoofSurface {
        id          : emptyUUID,
        polygon     : def,
        ridgePoints : Nil
        }
instance hasUUIDRoofSurface :: HasUUID RoofSurface where
    idLens = _id
_ridgePoints :: forall t a r. Newtype t { ridgePoints :: a | r } => Lens' t a
_ridgePoints = _Newtype <<< prop (SProxy :: SProxy "ridgePoints")

allPoints :: RoofSurface -> Array HousePoint
allPoints s = s ^. _polygon <<< _polyVerts

newSurface :: Polygon HousePoint -> Effect RoofSurface
newSurface poly = genUUID >>= flip newSurfaceWith poly >>> pure

newSurfaceWith :: UUID -> Polygon HousePoint -> RoofSurface
newSurfaceWith i poly = def # _id          .~ i
                            # _polygon     .~ poly
                            # _ridgePoints .~ ridgePoints (fromFoldable $ poly ^. _polyVerts)

-- | create a new RoofSurface around a point
surfaceAround :: Vector3 -> Effect RoofSurface
surfaceAround p = traverse mkHP (polygonAround 3.0 $ toVec2 p) >>= newSurface
    where mkHP v = newHousePoint RidgePoint (mkVec3 (vecX v) (vecY v) 0.01)
