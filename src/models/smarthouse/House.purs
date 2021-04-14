module Model.SmartHouse.House where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~), (^.))
import Data.List (List, singleton)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_floor, _height, _id, _name, _position, _roofs, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Math.Angle (Angle)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (Roof, createRoofFrom, renderRoof)
import Model.UUID (class HasUUID)
import Rendering.Node (Node, node, tapMesh)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
    roofs  :: List Roof
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
    roofPolys <- skeletonize slope $ singleton $ counterClockPoly poly
    roofs <- traverse createRoofFrom roofPolys
    
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        roofs  : roofs
        }

newtype HouseNode = HouseNode {
    roofTapped :: Event UUID,
    wallTapped :: Event Unit
    }

-- rendering
renderHouse :: House -> Node HouseTextureInfo HouseNode
renderHouse house = do
    let h = house ^. _height
        p = mkVec3 0.0 0.0 (meterVal h)
    wallTap <- renderWalls h $ house ^. _floor
    roofTap <- node (def # _position .~ pure p) $ traverse renderRoof $ house ^. _roofs
    pure $ HouseNode {
        roofTapped : anyEvt roofTap,
        wallTapped : wallTap
        }

renderWalls :: forall e. Meter -> Polygon Vector3 -> Node e (Event Unit)
renderWalls height poly = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    m <- tapMesh (def # _name .~ "walls") geo mat
    pure $ const unit <$> m ^. _tapped
