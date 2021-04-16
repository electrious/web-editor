module SmartHouse.HouseEditor where

import Prelude

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (Meter, meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_floor, _height, _name, _position, _roofs, _tapped)
import Editor.HeightEditor (dragArrowPos, setupHeightEditor)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Model.ActiveMode (ActiveMode)
import Model.Polygon (Polygon, _polyVerts)
import Model.SmartHouse.House (House, HouseNode(..), updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (renderRoof)
import Model.UUID (idLens)
import Rendering.Node (Node, fixNodeDWith, localEnv, node, tapMesh)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)

editHouse :: Dynamic ActiveMode -> House -> Node HouseTextureInfo HouseNode
editHouse actDyn house = do
    let h = house ^. _height
    fixNodeDWith h \hDyn -> do
        let mkP = mkVec3 0.0 0.0 <<< meterVal
            pDyn = mkP <$> hDyn
        wallTap <- renderWalls h $ house ^. _floor
        roofTap <- node (def # _position .~ pDyn) $ traverse renderRoof $ house ^. _roofs

        -- height editor
        let hPos = dragArrowPos $ house ^. _floor <<< _polyVerts
            updZ p z = mkVec3 (vecX p) (vecY p) (meterVal z)
            hPosDyn = updZ hPos <$> hDyn

        hEvt <- localEnv (const def) $ setupHeightEditor actDyn hPosDyn

        let nhEvt = flip updateHeight house <$> hEvt
        let hn = HouseNode {
            id         : house ^. idLens,
            roofTapped : anyEvt roofTap,
            wallTapped : wallTap,
            house      : nhEvt
            }
        pure { input: hEvt, output: hn }

renderWalls :: forall e. Meter -> Polygon Vector3 -> Node e (Event Unit)
renderWalls height poly = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    m <- tapMesh (def # _name .~ "walls") geo mat
    pure $ const unit <$> m ^. _tapped
