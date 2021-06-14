module SmartHouse.HouseEditor where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _floor, _height, _houseId, _id, _modeDyn, _name, _orientation, _panelType, _panels, _position, _roof, _roofs, _tapped, _updated)
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _heatmap, runHouseEditor)
import Editor.PanelLayer (_initPanels, _mainOrientation, _roofActive)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofManager (RoofsData, _racks, calcMainOrientation, getActivated)
import Editor.RoofNode (RoofNode, RoofNodeConfig, createRoofNode)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, latestEvt, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (delay, performEvent)
import Math.LineSeg (mkLineSeg)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Polygon (Polygon, _polyVerts)
import Model.Racking.OldRackingSystem (OldRoofRackingData, guessRackingType)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.Panel (Alignment(..), Orientation(..), PanelsDict, panelsDict)
import Model.Roof.RoofPlate (RoofPlate, _roofIntId)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), _roofTapped, _trees, _wallTapped, flipRoof, updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (_flipped, renderRoof)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeDWith, getEnv, localEnv, node, tapMesh)
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


newtype HouseEditorConf = HouseEditorConf {
    modeDyn        :: Dynamic ActiveMode,
    house          :: House,
    
    roofsData      :: Event RoofsData,
    arrayEditParam :: ArrayEditParam
    }

derive instance newtypeHouseEditorConf :: Newtype HouseEditorConf _
instance defaultHouseEditorConf :: Default HouseEditorConf where
    def = HouseEditorConf {
        modeDyn        : pure Inactive,
        house          : def,
        roofsData      : empty,
        arrayEditParam : def
        }

_house :: forall t a r. Newtype t { house :: a | r } => Lens' t a
_house = _Newtype <<< prop (SProxy :: SProxy "house")

_roofsData :: forall t a r. Newtype t { roofsData :: a | r } => Lens' t a
_roofsData = _Newtype <<< prop (SProxy :: SProxy "roofsData")

_arrayEditParam :: forall t a r. Newtype t { arrayEditParam :: a | r } => Lens' t a
_arrayEditParam = _Newtype <<< prop (SProxy :: SProxy "arrayEditParam")


-- whether the house editor is in the mode for house editing or
-- loaded all roofs/arrays for editing the arrays
data HouseEditorMode = EditingHouse
                     | EditingArrays

derive instance eqwHouseEditorMode :: Eq HouseEditorMode


editHouse :: HouseConfig -> HouseEditorConf -> Node HouseTextureInfo HouseNode
editHouse houseCfg conf = do
    let house  = conf ^. _house
        actDyn = conf ^. _modeDyn
        
        h      = house ^. _height
        floor  = house ^. _floor

        -- house editor mode
        modeDyn = step EditingHouse $ const EditingArrays <$> conf ^. _roofsData

        houseEditDyn = (==) EditingHouse <$> modeDyn
        
    fixNodeDWith house \houseDyn -> do
        let hDyn = view _height <$> houseDyn
            pDyn = mkVec3 0.0 0.0 <<< meterVal <$> hDyn
        -- render walls
        wallTap <- latestEvt <$> dynamic (renderWalls floor <$> hDyn)
        -- render roofs
        roofEvtsDyn <- node (def # _position .~ pDyn) $
                           dynamic $ traverse (renderRoof houseEditDyn actDyn) <<< view _roofs <$> houseDyn

        let flipEvt = latestAnyEvtWith (view _flipped) roofEvtsDyn
            -- height editor arrow position
            hPos2D = dragArrowPos $ house ^. _floor <<< _polyVerts
            hPos   = mkVec3 (vecX hPos2D) (vecY hPos2D) (meterVal h)

        -- setup height editor and get the delta event
        -- enable height editor only in EditingHouse mode and actDyn is active
        deltaEvt <- setupHeightEditor $ def # _modeDyn  .~ ((&&) <$> actDyn <*> (fromBoolean <$> houseEditDyn))
                                            # _position .~ pure hPos
                                            # _min      .~ (- meterVal h)
    
        let hEvt = (+) h <$> deltaEvt  -- new height
            newHouseEvt1 = sampleDyn houseDyn $ updateHeight <$> hEvt
            newHouseEvt2 = performEvent $ sampleDyn houseDyn $ flipRoof <$> flipEvt
            newHouseEvt  = newHouseEvt1 <|> newHouseEvt2
            hn = def # _id         .~ (house ^. idLens)
                     # _roofTapped .~ latestAnyEvtWith (view _tapped) roofEvtsDyn
                     # _wallTapped .~ wallTap
                     # _updated    .~ (HouseOpUpdate <$> newHouseEvt)

        -- render all roof nodes if available
            roofsDyn = step Nothing $ Just <$> conf ^. _roofsData

        void $ localEnv (const houseCfg) $ renderRoofs (conf ^. _arrayEditParam) roofsDyn
        
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


-- render roofNode on top of the house, which are used to edit arrays
renderRoofs :: ArrayEditParam -> Dynamic (Maybe RoofsData) -> Node HouseConfig (Dynamic (Array RoofNode))
renderRoofs param rdDyn = dynamic $ renderRd <$> rdDyn
    where renderRd Nothing   = pure []
          renderRd (Just rd) =
              fixNodeDWith Landscape \mainOrientDyn ->
                  fixNodeDWith Nothing \activeRoof -> do
                      houseId <- view _houseId <$> getEnv

                      let psDict = panelsDict $ rd ^. _panels
                          roofs  = rd ^. _roofs
                          racks  = rd ^. _racks

                          orientDyn  = step Landscape $ param ^. _orientation
                          alignDyn   = step Grid      $ param ^. _alignment
                          opacityDyn = step Opaque    $ param ^. _opacity
                  
                          cfg = def # _houseId         .~ houseId
                                    # _mainOrientation .~ mainOrientDyn
                                    # _orientation     .~ orientDyn
                                    # _alignment       .~ alignDyn
                                    # _panelType       .~ pure def
                                    # _opacity         .~ opacityDyn
                                    # _heatmap         .~ (param ^. _heatmap)

                      nodes <- traverse (mkRoofNode activeRoof psDict racks cfg) roofs
                      let mainOrientEvt = calcMainOrientation nodes
                          actRoofEvt    = Just <$> getActivated nodes
              
                      pure { input : actRoofEvt, output : { input : mainOrientEvt, output : nodes }}


mkRoofNode :: Dynamic (Maybe UUID) -> PanelsDict -> Map Int OldRoofRackingData -> RoofNodeConfig -> RoofPlate -> Node HouseConfig RoofNode
mkRoofNode activeRoof panelsDict racks cfg roof = do
    hCfg <- getEnv

    let rid = roof ^. _id
        ps  = fromMaybe Nil (lookup rid panelsDict)
        rackType    = fromMaybe XR10 $ guessRackingType <$> lookup (roof ^. _roofIntId) racks
        rackTypeDyn = pure rackType
        roofActive  = (==) (Just rid) <$> activeRoof
        
        roofNodeBuilder = createRoofNode $ cfg # _roof       .~ roof
                                               # _roofActive .~ roofActive
                                               # _initPanels .~ delay 100 (pure ps)
                                               
    liftEffect $ runHouseEditor (runArrayBuilder rackTypeDyn roofNodeBuilder) hCfg
