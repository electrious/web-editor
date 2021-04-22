module SmartHouse.HouseEditor where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.Default (def)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_floor, _height, _modeDyn, _name, _position, _roofs, _tapped)
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, latestEvt, sampleDyn)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode)
import Model.Polygon (Polygon, _polyVerts)
import Model.SmartHouse.House (House, HouseNode(..), HouseOp(..), flipRoof, updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (_flipped, renderRoof)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeDWith, node, tapMesh)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)
import Util (latestAnyEvtWith)

editHouse :: Dynamic ActiveMode -> House -> Node HouseTextureInfo HouseNode
editHouse actDyn house = do
    let h = house ^. _height
        floor = house ^. _floor
    fixNodeDWith house \houseDyn -> do
        let hDyn = view _height <$> houseDyn
            pDyn = mkVec3 0.0 0.0 <<< meterVal <$> hDyn
        -- render walls
        wallTap <- latestEvt <$> dynamic (renderWalls floor <$> hDyn)
        -- render roofs
        roofEvtsDyn <- node (def # _position .~ pDyn) $ dynamic $ traverse renderRoof <<< view _roofs <$> houseDyn

        let flipEvt = latestAnyEvtWith (view _flipped) roofEvtsDyn
        -- height editor arrow position
        let hPos2D = dragArrowPos $ house ^. _floor <<< _polyVerts
            hPos   = mkVec3 (vecX hPos2D) (vecY hPos2D) (meterVal h)

        -- setup height editor and get the delta event
        deltaEvt <- setupHeightEditor $ def # _modeDyn  .~ actDyn
                                            # _position .~ pure hPos
                                            # _min      .~ (- meterVal h)
    
        let hEvt = (+) h <$> deltaEvt  -- new height
            newHouseEvt1 = sampleDyn houseDyn $ updateHeight <$> hEvt
            newHouseEvt2 = performEvent $ sampleDyn houseDyn $ flipRoof <$> flipEvt
            newHouseEvt = newHouseEvt1 <|> newHouseEvt2
            hn = HouseNode {
                id          : house ^. idLens,
                roofTapped  : latestAnyEvtWith (view _tapped) roofEvtsDyn,
                wallTapped  : wallTap,
                updated     : HouseOpUpdate <$> newHouseEvt,
                deleted     : empty
                }
        pure { input: newHouseEvt, output: hn }

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    m <- tapMesh (def # _name .~ "walls") geo mat
    pure $ const unit <$> m ^. _tapped
