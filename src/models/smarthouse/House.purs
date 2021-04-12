module Model.SmartHouse.House where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~), (^.))
import Data.List (List, singleton)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse_)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_floor, _height, _id, _name, _position, _roofs)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import HouseBuilder.PolyGeometry (mkPolyGeometry)
import Math.Angle (Angle)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.UUID (class HasUUID)
import Rendering.Node (Node, mesh, node)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterialWithColor, mkMeshPhongMaterial)
import Three.Math.Color (Color, mkColorRGB)
import Three.Math.Vector (Vector3, mkVec3, toVec2)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
    roofs  :: List (Polygon Vector3)
    }

derive instance newtypeHouse :: Newtype House _
derive instance genericHouse :: Generic House _
instance showHouse :: Show House where
    show = genericShow
instance hasUUIDHouse :: HasUUID House where
    idLens = _id

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    roofs <- skeletonize slope $ singleton $ counterClockPoly poly
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        roofs  : roofs
        }


-- rendering
renderHouse :: forall e.House -> Node e Unit
renderHouse house = do
    let h = house ^. _height
        p = mkVec3 0.0 0.0 (meterVal h)
    renderWalls h $ house ^. _floor
    node (def # _position .~ pure p) $ traverse_ renderRoofPoly $ house ^. _roofs

randomColor :: Effect Color
randomColor = do
    r <- randomRange 0.0 1.0
    g <- randomRange 0.0 1.0
    b <- randomRange 0.0 1.0
    mkColorRGB r g b

randomRoofMaterial :: Effect MeshBasicMaterial
randomRoofMaterial = randomColor >>= mkMeshBasicMaterialWithColor

renderRoofPoly :: forall e. Polygon Vector3 -> Node e Unit
renderRoofPoly poly = do
    geo <- liftEffect $ mkPolyGeometry poly
    mat <- liftEffect randomRoofMaterial
    void $ mesh (def # _name .~ "roof") geo mat


renderWalls :: forall e. Meter -> Polygon Vector3 -> Node e Unit
renderWalls height poly = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    void $ mesh (def # _name .~ "walls") geo mat
