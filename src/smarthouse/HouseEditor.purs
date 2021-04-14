module SmartHouse.HouseEditor where

import Prelude

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (Meter, meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_floor, _height, _name, _position, _roofs, _tapped)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Model.Polygon (Polygon, _polyVerts)
import Model.SmartHouse.House (House, HouseNode(..))
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (renderRoof)
import Model.UUID (idLens)
import Rendering.Node (Node, node, tapMesh)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2)

editHouse :: Boolean -> House -> Node HouseTextureInfo HouseNode
editHouse active house = do
    let h = house ^. _height
        p = mkVec3 0.0 0.0 (meterVal h)
    wallTap <- renderWalls h $ house ^. _floor
    roofTap <- node (def # _position .~ pure p) $ traverse renderRoof $ house ^. _roofs
    pure $ HouseNode {
        id         : house ^. idLens,
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
