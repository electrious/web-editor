module SmartHouse.HouseEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Monad.RWS (tell)
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)
import Data.UUID (UUID)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _floor, _height, _houseId, _id, _modeDyn, _name, _orientation, _panelType, _panels, _position, _roof, _roofs, _shadeSelected, _tapped, _updated)
import Editor.Disposable (Disposee(..))
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _heatmap, runHouseEditor)
import Editor.PanelLayer (_initPanels, _mainOrientation, _roofActive)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofManager (RoofsData, _racks, calcMainOrientation, getActivated)
import Editor.RoofNode (RoofNode, RoofNodeConfig, RoofNodeMode(..), createRoofNode)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, dynEvent, gateDyn, latestEvt, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.LineSeg (mkLineSeg)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Polygon (Polygon, _polyVerts)
import Model.Racking.OldRackingSystem (OldRoofRackingData, guessRackingType)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.Panel (Alignment(..), Orientation(..), PanelsDict, panelsDict)
import Model.Roof.RoofPlate (RoofPlate, _roofIntId)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), _activeRoof, _roofTapped, _trees, _wallTapped, flipRoof, getRoof, getVertNode, updateActiveRoofShade, updateHeight)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (Roof, RoofEvents, _flipped, renderActRoofOutline, renderRoof)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic, dynamic_)
import Rendering.Line (renderLine, renderLineWith)
import Rendering.Node (Node, fixNodeDWith, getEnv, getParent, localEnv, node, tapMesh)
import SmartHouse.Algorithm.Edge (_line)
import SmartHouse.Algorithm.LAV (_edges)
import SmartHouse.BuilderMode (BuilderMode(..))
import SmartHouse.ShadeOption (ShadeOption)
import Smarthouse.Algorithm.Subtree (Subtree, _sinks, _source, treeLines)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshPhongMaterial, mkLineBasicMaterial, mkMeshPhongMaterial)
import Three.Core.Object3D (add, remove)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)
import Type.Proxy (Proxy(..))
import UI.RoofEditorUI (_mode)
import Util (latestAnyEvtWith)


-- how to render the House
data HouseRenderMode = EditHouseMode
                     | Render2DMode
derive instance eqHouseRenderMode :: Eq HouseRenderMode


newtype HouseEditorConf = HouseEditorConf {
    modeDyn        :: Dynamic ActiveMode,
    builderModeDyn :: Dynamic BuilderMode,
    house          :: House,

    shadeSelected  :: Event ShadeOption,
    
    roofsData      :: Event RoofsData,
    arrayEditParam :: ArrayEditParam
    }

derive instance newtypeHouseEditorConf :: Newtype HouseEditorConf _
instance defaultHouseEditorConf :: Default HouseEditorConf where
    def = HouseEditorConf {
        modeDyn        : pure Inactive,
        builderModeDyn : pure Building,
        house          : def,
        shadeSelected  : empty,
        
        roofsData      : empty,
        arrayEditParam : def
        }

_builderModeDyn :: forall t a r. Newtype t { builderModeDyn :: a | r } => Lens' t a
_builderModeDyn = _Newtype <<< prop (Proxy :: Proxy "builderModeDyn")

_house :: forall t a r. Newtype t { house :: a | r } => Lens' t a
_house = _Newtype <<< prop (Proxy :: Proxy "house")

_roofsData :: forall t a r. Newtype t { roofsData :: a | r } => Lens' t a
_roofsData = _Newtype <<< prop (Proxy :: Proxy "roofsData")

_arrayEditParam :: forall t a r. Newtype t { arrayEditParam :: a | r } => Lens' t a
_arrayEditParam = _Newtype <<< prop (Proxy :: Proxy "arrayEditParam")


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
        
    fixNodeDWith house \houseDyn ->
        fixNodeDWith Nothing \actRoofIdDyn -> do
            let hDyn = view _height <$> houseDyn
                pDyn = mkVec3 0.0 0.0 <<< meterVal <$> hDyn
            -- render walls
            wallTap <- latestEvt <$> dynamic (renderWalls floor <$> hDyn)

            -- calculate the active roof id based on house's activeness and active roof id
            let f Active   i = i
                f Inactive _ = Nothing

                g (Just i) m = M.lookup i m
                g Nothing _  = Nothing

                -- id of active roof, taking into account of the house activeness
                actRoofDyn = f <$> actDyn <*> actRoofIdDyn

            -- render roofs
            roofEvtsDyn <- node (def # _position .~ pDyn
                                     # _name     .~ "roofs") do
                let roofsDyn = view _roofs <$> houseDyn
                    actRDyn = g <$> actRoofDyn <*> roofsDyn
                    buildModeDyn = conf ^. _builderModeDyn
                node (def # _name .~ "roof-lines"
                          # _position .~ pure (mkVec3 0.0 0.0 0.02)) do
                    -- render ridge lines
                    dynamic_ $ renderSubtrees <$> houseDyn <*> buildModeDyn
                    -- render edge lines
                    dynamic_ $ renderEdgeLines <$> houseDyn <*> buildModeDyn
                    -- render roof outlines dynamically
                    dynamic_ $ renderActRoofOutline <$> buildModeDyn <*> actRDyn

                -- render roofs dynamically
                dynamic $ renderBuilderRoofs houseEditDyn actRoofDyn <$> roofsDyn


            let flipEvt = latestAnyEvtWith (view _flipped) roofEvtsDyn
                -- height editor arrow position
                hPos2D = dragArrowPos $ house ^. _floor <<< _polyVerts
                hPos   = mkVec3 (vecX hPos2D) (vecY hPos2D) (meterVal h)

            -- setup height editor and get the delta event
            -- enable height editor only in EditingHouse mode and actDyn is active
            hEvt <- setupHeightEditor $ def # _modeDyn  .~ ((&&) <$> actDyn <*> (fromBoolean <$> houseEditDyn))
                                            # _position .~ hPos
                                            # _height   .~ h
                                            # _min      .~ (- meterVal h)
    
            let newHouseEvt1 = sampleDyn houseDyn $ updateHeight <$> hEvt
                newHouseEvt2 = performEvent $ sampleDyn houseDyn $ flipRoof <$> flipEvt
                newHouseEvt3 = sampleDyn houseDyn $ sampleDyn actRoofIdDyn $ updateActiveRoofShade <$> conf ^. _shadeSelected
                
                newHouseEvt  = multicast $ newHouseEvt1 <|> newHouseEvt2 <|> newHouseEvt3

                roofTappedEvt = latestAnyEvtWith (view _tapped) roofEvtsDyn

                activeRoofDyn = getRoof <$> actRoofDyn <*> houseDyn
                
                hn = def # _id         .~ (house ^. idLens)
                         # _roofTapped .~ gateDyn (not <<< isActive <$> actDyn) roofTappedEvt
                         # _wallTapped .~ wallTap
                         # _updated    .~ (HouseOpUpdate <$> newHouseEvt)
                         # _activeRoof .~ dynEvent activeRoofDyn

                -- render all roof nodes if available
                roofsDyn = step Nothing $ Just <$> conf ^. _roofsData

            void $ localEnv (const houseCfg) $ renderRoofEditor (conf ^. _arrayEditParam) roofsDyn
            
            pure { input : Just <$> roofTappedEvt, output : { input: newHouseEvt, output: hn } }


wallMat :: MeshPhongMaterial
wallMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    m <- tapMesh (def # _name .~ "walls") geo wallMat
    pure $ const unit <$> m ^. _tapped

-- render all subtree lines with length text
renderSubtrees :: forall e. House -> BuilderMode -> Node e Unit
renderSubtrees h Building = traverse_ (renderSubtree h) (h ^. _trees)
renderSubtrees _ Showing  = pure unit

renderSubtree :: forall e. House -> Subtree -> Node e Unit
renderSubtree h t = traverse_ renderLine ls
    where ls = map (view _position <<< flip getVertNode h) <$> treeLines t


-- render all house edge lines
renderEdgeLines :: forall e. House -> BuilderMode -> Node e Unit
renderEdgeLines h Building = traverse_ renderLine (view _line <$> h ^. _edges)
renderEdgeLines _ Showing  = pure unit

renderBuilderRoofs :: forall f. Traversable f => Dynamic Boolean -> Dynamic (Maybe UUID) -> f Roof -> Node HouseTextureInfo (f RoofEvents)
renderBuilderRoofs houseEditDyn actRoofDyn = traverse (renderRoof houseEditDyn actRoofDyn)

-- render the house as 2D wireframe
renderHouse :: House -> Node HouseTextureInfo HouseNode
renderHouse house = do
    traverse_ renderLine $ view _line <$> house ^. _edges

    let mkLines t = mkLineSeg (t ^. _source <<< _position) <<< view _position <$> t ^. _sinks
        renderTree t = do
            c <- liftEffect $ randomInt 0 0xffffff
            mat <- liftEffect $ mkLineBasicMaterial c 4.0
            traverse_ (flip renderLineWith mat) $ mkLines t
    traverse_ renderTree $ house ^. _trees
        
    pure (def # _id .~ (house ^. idLens))


-- render roofNode on top of the house, which are used to edit arrays
renderRoofEditor :: ArrayEditParam -> Dynamic (Maybe RoofsData) -> Node HouseConfig (Dynamic (Array RoofNode))
renderRoofEditor param rdDyn = dynamic $ renderRd <$> rdDyn
    where renderRd Nothing   = pure []
          renderRd (Just rd) =
              fixNodeDWith Landscape \mainOrientDyn ->
                  fixNodeDWith Nothing \activeRoof -> do
                      let psDict = panelsDict $ rd ^. _panels
                          roofs  = rd ^. _roofs
                          racks  = rd ^. _racks

                          orientDyn  = step Landscape $ param ^. _orientation
                          alignDyn   = step Grid      $ param ^. _alignment
                          opacityDyn = step Opaque    $ param ^. _opacity
                  
                          cfg = def # _mode            .~ RoofInBuilder
                                    # _houseId         .~ (rd ^. _houseId)
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
                                               
    n <- liftEffect $ runHouseEditor (runArrayBuilder rackTypeDyn roofNodeBuilder) hCfg

    -- add the new node object to the current Node context's parent element
    -- and setup the dispose action
    parent <- getParent
    liftEffect $ add n parent

    tell $ Disposee $ remove n parent

    pure n
