module SmartHouse.HouseEditor where

import Prelude

import Control.Alternative (empty)
import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (Meter, meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_floor, _height, _modeDyn, _name, _position, _roofs, _tapped)
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, latestEvt)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Model.ActiveMode (ActiveMode)
import Model.Polygon (Polygon, _polyVerts)
import Model.SmartHouse.House (House, HouseNode(..), HouseOp(..), updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (renderRoof)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeDWith, node, tapMesh)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)

editHouse :: Dynamic ActiveMode -> House -> Node HouseTextureInfo HouseNode
editHouse actDyn house = do
    let h = house ^. _height
    fixNodeDWith h \hDyn -> do
        let pDyn = mkVec3 0.0 0.0 <<< meterVal <$> hDyn
        wallTap <- latestEvt <$> dynamic (renderWalls (house ^. _floor) <$> hDyn)
        roofTap <- node (def # _position .~ pDyn) $ traverse renderRoof $ house ^. _roofs

        -- height editor
        let hPos2D = dragArrowPos $ house ^. _floor <<< _polyVerts
            hPos   = mkVec3 (vecX hPos2D) (vecY hPos2D) (meterVal h)
        hEvt <- setupHeightEditor $ def # _modeDyn .~ actDyn
                                        # _position .~ pure hPos
                                        # _min .~ (- meterVal h)
    
        let nhEvt = (+) h <$> hEvt
            newHouseEvt = flip updateHeight house <$> hEvt
            hn = HouseNode {
                id         : house ^. idLens,
                roofTapped : anyEvt roofTap,
                wallTapped : wallTap,
                updated    : HouseOpUpdate <$> newHouseEvt,
                deleted    : empty
                }
        pure { input: nhEvt, output: hn }

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    m <- tapMesh (def # _name .~ "walls") geo mat
    pure $ const unit <$> m ^. _tapped
