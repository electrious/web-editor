module SmartHouse.HouseEditor where

import Prelude

import Control.Alt ((<|>))
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, meterVal)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_floor, _height, _id, _modeDyn, _name, _position, _roofs, _tapped, _updated)
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, latestEvt, sampleDyn)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Math.LineSeg (mkLineSeg)
import Model.ActiveMode (ActiveMode)
import Model.Polygon (Polygon, _polyVerts)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), _roofTapped, _trees, _wallTapped, flipRoof, updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (_flipped, renderRoof)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeDWith, node, tapMesh)
import SmartHouse.Algorithm.Edge (_line)
import SmartHouse.Algorithm.LAV (_edges)
import SmartHouse.HouseTracer (renderLine, renderLineWith)
import Smarthouse.Algorithm.Subtree (_sinks, _source)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshPhongMaterial, mkLineBasicMaterial, mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)
import Util (latestAnyEvtWith)


-- how to render the House
data HouseRenderMode = EditHouseMode
                     | Render2DMode
derive instance eqHouseRenderMode :: Eq HouseRenderMode

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
        roofEvtsDyn <- node (def # _position .~ pDyn) $ dynamic $ traverse (renderRoof actDyn) <<< view _roofs <$> houseDyn

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
            hn = def # _id         .~ (house ^. idLens)
                     # _roofTapped .~ latestAnyEvtWith (view _tapped) roofEvtsDyn
                     # _wallTapped .~ wallTap
                     # _updated    .~ (HouseOpUpdate <$> newHouseEvt)
        pure { input: newHouseEvt, output: hn }


wallMat :: MeshPhongMaterial
wallMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    m <- tapMesh (def # _name .~ "walls") geo wallMat
    pure $ const unit <$> m ^. _tapped


-- render the house as 2D wireframe
renderHouse :: House -> Node HouseTextureInfo HouseNode
renderHouse house = do
    traverse_ renderLine $ view _line <$> house ^. _edges

    let mkLines t = mkLineSeg (t ^. _source) <$> t ^. _sinks
        renderTree t = do
            c <- liftEffect $ randomInt 0 0xffffff
            mat <- liftEffect $ mkLineBasicMaterial c 4.0
            traverse_ (flip renderLineWith mat) $ mkLines t
    traverse_ renderTree $ house ^. _trees
        
    pure (def # _id .~ (house ^. idLens))
