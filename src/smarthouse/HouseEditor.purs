module SmartHouse.HouseEditor where

import Prelude hiding (add)

import API (APIConfig)
import API.Racking (RackRequest, _parameters, doRack, runRackAPI)
import Control.Alt ((<|>))
import Control.Monad.RWS (tell)
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.UUIDMap (UUIDMap)
import Data.UUIDWrapper (UUID)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _apiConfig, _buildChimney, _chimney, _edges, _floor, _height, _houseId, _id, _modeDyn, _mouseMove, _name, _orientation, _panelType, _panels, _position, _roof, _roofRackings, _roofs, _slopeSelected, _tapped, _updated)
import Editor.Disposable (Disposee(..))
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _heatmap, runHouseEditor)
import Editor.PanelLayer (_initPanels, _mainOrientation, _roofActive)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofManager (RoofsData, allPanelsEvt, calcMainOrientation, getActivated)
import Editor.RoofNode (RoofNode, RoofNodeConfig, RoofNodeMode(..), _racking, createRoofNode)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, distinctDyn, dynEvent, gateDyn, latestEvt, sampleDyn, step)
import FRP.Event (Event, sampleOn)
import FRP.Event as Evt
import FRP.Event.Extra (debounce, delay, multicast, performEvent)
import Math.LineSeg (LineSeg, mkLineSeg)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Polygon (Polygon, _polyVerts)
import Model.Racking.RackingSystem (RackingSystem)
import Model.Racking.RackingType (RackingType(..))
import Model.Racking.RoofParameter (RoofParameter(..))
import Model.Roof.Panel (Alignment(..), Orientation(..), Panel, PanelsDict, panelsDict)
import Model.Roof.RoofPlate (RoofPlate)
import Model.SmartHouse.Chimney (Chimney, ChimneyNode, ChimneyOp(..))
import Model.SmartHouse.House (House, _chimneys, _trees, getHouseLines, updateHeight, updateHouseChimney, updateHouseSlope)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (Roof)
import Model.UUID (idLens)
import Models.SmartHouse.ActiveItem (ActHouseItem)
import Rendering.DynamicNode (dynamic, dynamic_)
import Rendering.Line (renderLine, renderLineLength, renderLineOnly, renderLineWith)
import Rendering.Node (Node, _exportable, fixNodeD2With, fixNodeE, getEnv, getParent, localEnv, node, tapMesh)
import SmartHouse.Algorithm.Edge (_lineEdge)
import SmartHouse.ChimneyBuilder (addChimney, editChimney)
import SmartHouse.RoofNode (RoofMesh, renderActRoofOutline, renderRoof)
import SmartHouse.SlopeOption (SlopeOption)
import Smarthouse.Algorithm.Subtree (_sinks, _source)
import Smarthouse.HouseNode (HouseNode, HouseOp(..), _actHouseItem, _activated)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshPhongMaterial, mkLineBasicMaterial, mkMeshPhongMaterial)
import Three.Core.Object3D (add, remove)
import Three.Math.Vector (Vector3, mkVec3, toVec2, vecX, vecY)
import Type.Proxy (Proxy(..))
import UI.RoofEditorUI (_mode)
import Util (debounceDyn, latestAnyEvtWith)


-- how to render the House
data HouseRenderMode = EditHouseMode
                     | Render2DMode
derive instance eqHouseRenderMode :: Eq HouseRenderMode


newtype HouseEditorConf = HouseEditorConf {
    modeDyn        :: Dynamic ActiveMode,
    house          :: House,

    slopeSelected  :: Event SlopeOption,
    buildChimney   :: Event Boolean,
    
    roofsData      :: Event RoofsData,
    arrayEditParam :: ArrayEditParam
    }

derive instance newtypeHouseEditorConf :: Newtype HouseEditorConf _
instance defaultHouseEditorConf :: Default HouseEditorConf where
    def = HouseEditorConf {
        modeDyn        : pure Inactive,
        house          : def,
        slopeSelected  : empty,
        buildChimney   : empty,
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


getActItem :: Maybe UUID -> House -> ActHouseItem
getActItem Nothing h  = def # _house .~ h
getActItem (Just i) h = def # _house .~ h
                            # _roof  .~ M.lookup i (h ^. _roofs)
                            # _chimney .~ M.lookup i (h ^. _chimneys)

activeId :: ActiveMode -> Maybe UUID -> Maybe UUID
activeId Active   i = i
activeId Inactive _ = Nothing

getActiveRoof :: Maybe UUID -> UUIDMap Roof -> Maybe Roof
getActiveRoof i m = join $ flip M.lookup m <$> i

-- | build rack request with panels using default XRParameter and panel size
rackRequest :: Array Panel -> RoofsData -> RackRequest
rackRequest ps rd = def # _parameters .~ params
                        # _panels     .~ ps
    where params = M.fromFoldable $ (\r -> Tuple (r ^. idLens) param) <$> rd ^. _roofs
          param = XRParameter def


renderRoofs :: Dynamic Vector3 -> Dynamic House -> Dynamic (Maybe UUID) -> Dynamic Boolean -> Dynamic Boolean -> Node HouseTextureInfo (Dynamic (UUIDMap RoofMesh))
renderRoofs pDyn houseDyn actRoofDyn houseEditDyn chimEditDyn = 
    node (def # _position .~ pDyn
              # _name     .~ "roofs") do
        let roofsDyn = view _roofs <$> houseDyn
            actRDyn = getActiveRoof <$> actRoofDyn <*> roofsDyn
        node (def # _name .~ "roof-lines"
                  # _position .~ pure (mkVec3 0.0 0.0 0.02)) do
            -- render ridge lines
            let linesDyn = getHouseLines <$> houseDyn
            dynamic_ $ traverse_ renderLineOnly <$> linesDyn
            -- render line length texts
            dynamic_ $ renderLengths <$> debounceDyn (Milliseconds 500.0) linesDyn

            -- render roof outlines dynamically
            dynamic_ $ renderActRoofOutline <$> actRDyn

        -- render roofs dynamically
        dynamic $ renderBuilderRoofs houseEditDyn chimEditDyn <$> roofsDyn

renderChimneys :: forall e. Dynamic Vector3 -> Dynamic ActiveMode -> Dynamic House -> Dynamic (Maybe UUID) -> Node e (Dynamic (UUIDMap ChimneyNode))
renderChimneys pDyn actDyn hsDyn actIdDyn =
    node (def # _name .~ "chimneys"
              # _position .~ pDyn) do
        let chimsDyn = distinctDyn $ view _chimneys <$> hsDyn
            f c Active (Just i) = if c ^. idLens == i then Active else Inactive
            f _ Active Nothing  = Inactive
            f _ Inactive _      = Inactive
            edit c = do
                let d = f c <$> actDyn <*> actIdDyn
                editChimney d c
        dynamic $ traverse edit <$> chimsDyn

houseWithNewHeight :: forall e. Dynamic ActiveMode -> Dynamic House -> Node e (Event House)
houseWithNewHeight canEditDyn houseDyn = do
    house <- liftEffect $ current houseDyn
    let h = house ^. _height
        floor  = view _position <$> house ^. _floor

        -- height editor arrow position
        hPos2D = dragArrowPos $ floor ^. _polyVerts
        hPos   = mkVec3 (vecX hPos2D) (vecY hPos2D) (meterVal h)

    -- setup height editor and get the height event
    -- enable height editor only in EditingHouse mode and actDyn is active
    hEvt <- setupHeightEditor $ def # _modeDyn  .~ canEditDyn
                                    # _position .~ hPos
                                    # _height   .~ h
                                    # _min      .~ (- meterVal h)

    pure $ sampleDyn houseDyn $ updateHeight <$> hEvt


houseWithNewSlope :: Dynamic ActiveMode -> Event SlopeOption -> Dynamic House -> Dynamic (Maybe UUID) -> Event House
houseWithNewSlope canEditDyn sEvt houseDyn actRoofIdDyn = performEvent $ sampleDyn houseDyn $ sampleDyn actRoofIdDyn $ updateHouseSlope <$> slopeEvt
    -- only accept slope events if the house is active and can be edit
    where slopeEvt = gateDyn (isActive <$> canEditDyn) sEvt

houseWithNewChim :: Dynamic House -> Event Chimney -> Event ChimneyOp -> Event House
houseWithNewChim houseDyn chEvt chOpEvt = sampleDyn houseDyn $ updateHouseChimney <$> opEvt
    where opEvt = chOpEvt <|> (ChimCreate <$> chEvt)

rackSysForNewPanels :: Dynamic APIConfig -> Event RoofsData -> Event (Array Panel) -> Event RackingSystem
rackSysForNewPanels apiCfg roofsEvt panelsEvt = Evt.keepLatest $ performEvent $ sampleDyn apiCfg (runRackAPI <<< doRack <$> reqEvt)
    where reqEvt = sampleOn roofsEvt $ rackRequest <$> debounce (Milliseconds 100.0) panelsEvt


editHouse :: HouseConfig -> HouseEditorConf -> Node HouseTextureInfo HouseNode
editHouse houseCfg conf = do
    let house  = conf ^. _house
        -- house is activated or not
        actDyn = conf ^. _modeDyn
        
        floor  = view _position <$> house ^. _floor

        roofsEvt = multicast $ conf ^. _roofsData
        -- house can be edited until roofplates loaded and turn in to array edit mode
        houseEditDyn = step true $ const false <$> roofsEvt
        -- build chimney mode or not
        buildChimDyn = step false $ conf ^. _buildChimney
        chimActDyn   = fromBoolean <$> ((&&) <$> houseEditDyn <*> buildChimDyn)

    fixNodeD2With house Nothing \houseDyn actIdDyn ->
        fixNodeE \panelsEvt -> do
            let hDyn = view _height <$> houseDyn
                pDyn = mkVec3 0.0 0.0 <<< meterVal <$> hDyn
            -- render walls
            wallTap <- latestEvt <$> dynamic (renderWalls floor <$> hDyn)

            -- calculate the active roof id based on house's activeness and active roof id
            let -- id of active roof, taking into account of the house activeness
                actItemIdDyn = activeId <$> actDyn <*> actIdDyn
                
                canEditDyn = (&&) <$> actDyn <*> (fromBoolean <$> houseEditDyn)

            -- render roofs
            roofMeshesDyn <- renderRoofs pDyn houseDyn actItemIdDyn houseEditDyn buildChimDyn

            -- render chimneys
            chimNodesDyn <- renderChimneys pDyn chimActDyn houseDyn actIdDyn
            let chimTapEvt = multicast $ latestAnyEvtWith (view _tapped) chimNodesDyn
                chimUpdEvt = multicast $ latestAnyEvtWith (view _updated) chimNodesDyn

            let roofMouseEvt = latestAnyEvtWith (view _mouseMove) roofMeshesDyn
            chimEvt <- addChimney chimActDyn roofMouseEvt

            newHouseEvt1 <- houseWithNewHeight canEditDyn houseDyn
            let newHouseEvt2 = houseWithNewSlope canEditDyn (conf ^. _slopeSelected) houseDyn actIdDyn
                newHouseEvt3 = houseWithNewChim houseDyn chimEvt chimUpdEvt
                newHouseEvt  = multicast $ newHouseEvt1 <|> newHouseEvt2 <|> newHouseEvt3

                roofTappedEvt = multicast $ latestAnyEvtWith (view _tapped) roofMeshesDyn
                validRoofTappedEvt = gateDyn (not <<< isActive <$> actDyn) roofTappedEvt
                wallTappedEvt = const (house ^. idLens) <$> wallTap

                hn = def # _id           .~ (house ^. idLens)
                         # _activated    .~ (validRoofTappedEvt <|> wallTappedEvt)
                         # _updated      .~ (HouseOpUpdate <$> newHouseEvt)
                         # _actHouseItem .~ dynEvent (getActItem <$> actItemIdDyn <*> houseDyn)

                -- render all roof nodes if available
                roofsDyn = step Nothing $ Just <$> roofsEvt

                rackEvt = rackSysForNewPanels (houseCfg ^. _apiConfig) roofsEvt panelsEvt
                
            nodesDyn <- localEnv (const houseCfg) $ renderRoofEditor (conf ^. _arrayEditParam) roofsDyn rackEvt

            let newPanelsEvt = multicast $ latestEvt $ allPanelsEvt <$> nodesDyn
            
            pure {input : newPanelsEvt, output : { input1: newHouseEvt, input2: Just <$> (roofTappedEvt <|> chimTapEvt), output: hn } }


wallMat :: MeshPhongMaterial
wallMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    m <- tapMesh (def # _name       .~ "walls"
                      # _exportable .~ true) geo wallMat
    pure $ const unit <$> m ^. _tapped


renderLengths :: forall e. Maybe (List (LineSeg Vector3)) -> Node e Unit
renderLengths (Just ls) = traverse_ renderLineLength ls
renderLengths Nothing   = pure unit

renderBuilderRoofs :: forall f. Traversable f => Dynamic Boolean -> Dynamic Boolean -> f Roof -> Node HouseTextureInfo (f RoofMesh)
renderBuilderRoofs houseEditDyn chimEditDyn = traverse (renderRoof houseEditDyn chimEditDyn)

-- render the house as 2D wireframe
renderHouse :: House -> Node HouseTextureInfo HouseNode
renderHouse house = do
    traverse_ renderLine $ view _lineEdge <$> house ^. _edges

    let mkLines t = mkLineSeg (t ^. _source <<< _position) <<< view _position <$> t ^. _sinks
        renderTree t = do
            c <- liftEffect $ randomInt 0 0xffffff
            mat <- liftEffect $ mkLineBasicMaterial c 4.0
            traverse_ (flip renderLineWith mat) $ mkLines t
    traverse_ renderTree $ house ^. _trees
        
    pure (def # _id .~ (house ^. idLens))


-- render roofNode on top of the house, which are used to edit arrays
renderRoofEditor :: ArrayEditParam -> Dynamic (Maybe RoofsData) -> Event RackingSystem -> Node HouseConfig (Dynamic (Array RoofNode))
renderRoofEditor param rdDyn rackSysEvt = dynamic $ renderRd <$> rdDyn
    where renderRd Nothing   = pure []
          renderRd (Just rd) =
              fixNodeD2With Landscape Nothing \mainOrientDyn activeRoof -> do
                  let psDict = panelsDict $ rd ^. _panels
                      roofs  = rd ^. _roofs

                      orientDyn  = step Landscape $ param ^. _orientation
                      alignDyn   = step Grid      $ param ^. _alignment
                      opacityDyn = step Opaque    $ param ^. _opacity

                      cfg r = def # _mode            .~ RoofInBuilder
                                  # _houseId         .~ (rd ^. _houseId)
                                  # _mainOrientation .~ mainOrientDyn
                                  # _orientation     .~ orientDyn
                                  # _alignment       .~ alignDyn
                                  # _panelType       .~ pure def
                                  # _opacity         .~ opacityDyn
                                  # _racking         .~ step Nothing (M.lookup (r ^. _id) <<< view _roofRackings <$> rackSysEvt)
                                  # _heatmap         .~ (param ^. _heatmap)

                  nodes <- traverse (\r -> mkRoofNode activeRoof psDict (cfg r) r) roofs
                  let mainOrientEvt = calcMainOrientation nodes
                      actRoofEvt    = Just <$> getActivated nodes

                  pure { input1 : mainOrientEvt, input2: actRoofEvt, output : nodes }


mkRoofNode :: Dynamic (Maybe UUID) -> PanelsDict -> RoofNodeConfig -> RoofPlate -> Node HouseConfig RoofNode
mkRoofNode activeRoof panelsDict cfg roof = do
    hCfg <- getEnv

    let rid = roof ^. _id
        ps  = fromMaybe Nil (lookup rid panelsDict)
        rackType    = XR10
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
