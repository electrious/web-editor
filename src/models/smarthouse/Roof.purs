module Model.SmartHouse.Roof where

import Prelude hiding (degree)

import Algorithm.Plane (Plane)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (^.))
import Data.List (List)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_id, _polygon, _slope)
import Math.Angle (Angle, degree)
import Model.Polygon (Polygon, _polyVerts, polyPlane)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)

data RoofState = SlopeRoof
               | Gable

derive instance Eq RoofState

newtype Roof = Roof {
    id       :: UUID,
    polygon  :: Polygon Vector3,

    edges    :: List Edge,
    slope    :: Angle,

    normal   :: Vector3
    }

derive instance Newtype Roof _
derive instance Generic Roof _
instance Show Roof where
    show = genericShow
instance Eq Roof where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance HasUUID Roof where
    idLens = _id

createRoofFrom :: UUID -> Polygon Vector3 -> List Edge -> Vector3 -> Angle -> Roof
createRoofFrom i p es n s = Roof { id : i, polygon : p, edges : es, slope: s, normal : n }

roofState :: Roof -> RoofState
roofState r = if r ^. _slope > degree 89.0 then Gable else SlopeRoof

roofPlane :: Roof -> Plane
roofPlane = polyPlane <<< view _polygon

exportRoof :: Meter -> Roof -> JSRoof
exportRoof h r = JSRoof { id: r ^. idLens, polygon: mkP <$> r ^. _polygon <<< _polyVerts }
    where hv = meterVal h
          mkP v = vec2Point $ mkVec3 (vecX v) (vecY v) (vecZ v + hv)


-- The Roof data structure saved to server
newtype JSRoof = JSRoof {
    id      :: UUID,
    polygon :: Array Point
    }

derive instance Generic JSRoof _
instance Show JSRoof where
    show = genericShow
instance EncodeJson JSRoof where
    encodeJson (JSRoof r) = "id" := r.id
                         ~> "polygon" := r.polygon
                         ~> jsonEmptyObject
instance DecodeJson JSRoof where
    decodeJson = decodeJson >=> f
        where f o = mkJSRoof <$> o .: "id"
                             <*> o .: "polygon"

mkJSRoof :: UUID -> Array Point -> JSRoof
mkJSRoof id polygon = JSRoof { id, polygon }
