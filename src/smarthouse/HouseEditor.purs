module SmartHouse.HouseEditor where

import Prelude hiding (add)

import API (APIConfig)
import API.Racking (RackRequest, _parameters, doRack, runRackAPI)
import Control.Alt ((<|>))
import Control.Monad.RWS (tell)
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (traverse_)
import Data.Lens (Lens', set, view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.UUIDMap (UUIDMap)
import Data.UUIDWrapper (UUID)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_alignment, _apiConfig, _buildChimney, _chimney, _chimneys, _delChimney, _edges, _floor, _height, _houseId, _id, _modeDyn, _mouseMove, _name, _orientation, _panelType, _panels, _position, _roof, _roofId, _roofRackings, _roofs, _slopeSelected, _tapped, _updated)
import Taihe.Disposable (Disposee(..))
import Editor.HeightEditor (_min, dragArrowPos, setupHeightEditor)
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _heatmap, runHouseEditor)
import Editor.PanelLayer (_initPanels, _mainOrientation, _roofActive)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Editor.RoofManager (RoofsData, allPanelsEvt, calcMainOrientation, getActivated)
import Editor.RoofNode (RoofNode, RoofNodeConfig, RoofNodeMode(..), _racking, createRoofNode)
import Taihe.SceneEvent (SceneMouseMoveEvent)
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
import Model.SmartHouse.House (House, _trees, getHouseLines, updateHeight, updateHouseSlope)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (Roof)
import Model.UUID (idLens)
import Models.SmartHouse.ActiveItem (ActHouseItem)
import Taihe.DynamicNode (dynamic, dynamic_)
import Rendering.Line (renderLine, renderLineLength, renderLineOnly, renderLineWith)
import Taihe.Node (Node, _exportable, fixNodeD2With, fixNodeE, getEnv, getParent, localEnv, node, tapMesh)
import SmartHouse.Algorithm.Edge (_lineEdge)
import SmartHouse.ChimneyBuilder (addChimney, editChimney)
import SmartHouse.RoofNode (renderActRoofOutline, renderRoof)
import SmartHouse.SlopeOption (SlopeOption)
import Smarthouse.Algorithm.Subtree (_sinks, _source)
import Smarthouse.HouseNode (HouseNode, HouseOp(..), _actHouseItem, _activated)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry)
import Three.Core.Material (MeshPhongMaterial, mkLineBasicMaterial, mkMeshPhongMaterial)
import Three.Core.Object3D (add, remove)
import Three.Math.Shape (mkShapeWith)
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
    delChimney     :: Event UUID,
    
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
        delChimney     : empty,
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


getActItem :: Maybe UUID -> House -> Maybe Chimney -> ActHouseItem
getActItem Nothing h c = def # _house .~ h
                             # _chimney .~ c
getActItem (Just i) h c = def # _house .~ h
                              # _roof  .~ M.lookup i (h ^. _roofs)
                              # _chimney .~ c

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


newtype RoofEvts = RoofEvts {
    tapped  :: Event UUID,
    chimney :: ChimneyEvts
}

derive instance Newtype RoofEvts _

_activeChim :: forall t a r. Newtype t { activeChim :: a | r } => Lens' t a
_activeChim = _Newtype <<< prop (Proxy :: Proxy "activeChim")

renderRoofs :: Dynamic Vector3 -> Dynamic House -> Dynamic (Maybe UUID) -> Dynamic ActiveMode -> UUIDMap Chimney -> Event UUID -> Dynamic Boolean -> Node HouseTextureInfo RoofEvts
renderRoofs pDyn houseDyn actRoofDyn houseModeDyn chims delChimEvt chimEditDyn = 
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
        m <- dynamic $ traverse renderRoof <$> roofsDyn

        let mouseEvt = latestAnyEvtWith (view _mouseMove) m
            tapEvt   = multicast $ latestAnyEvtWith (view _tapped) m
        chimEvts <- editChimneys houseModeDyn chimEditDyn chims delChimEvt mouseEvt tapEvt

        pure $ RoofEvts {
            tapped  : tapEvt,
            chimney : chimEvts
        }

updateChimneys :: ChimneyOp -> UUIDMap Chimney -> UUIDMap Chimney
updateChimneys (ChimCreate c) = M.insert (c ^. idLens) c
updateChimneys (ChimUpdate c) = M.update (const $ Just c) (c ^. idLens)
updateChimneys (ChimDelete c) = M.delete c

renderChimneys :: forall e f. Traversable f => Dynamic ActiveMode -> Dynamic (Maybe UUID) -> f Chimney -> Node e (f ChimneyNode)
renderChimneys actDyn actIdDyn = traverse f
    where f c = editChimney (g c <$> actDyn <*> actIdDyn) c
          g c Active (Just i) = if c ^. _id == i then Active else Inactive
          g _ Active Nothing  = Inactive
          g _ Inactive _      = Inactive


newtype ChimneyEvts = ChimneyEvts {
    chimneys :: Event (UUIDMap Chimney),
    activeChim :: Event (Maybe Chimney)
}

derive instance Newtype ChimneyEvts _
instance Default ChimneyEvts where
    def = ChimneyEvts {
        chimneys : empty,
        activeChim : empty
    }

editChimneys :: forall e. Dynamic ActiveMode -> Dynamic Boolean -> UUIDMap Chimney -> Event UUID -> Event (Tuple UUID SceneMouseMoveEvent) -> Event UUID -> Node e ChimneyEvts
editChimneys houseModeDyn chimEditDyn chims delEvt mouseEvt roofTapEvt = do
    let actDyn =(&&) <$> houseModeDyn <*> (fromBoolean <$> chimEditDyn)

    fixNodeD2With chims Nothing \chimsDyn actIdDyn -> do
        -- render all chimneys
        chimNodesDyn <- dynamic $ renderChimneys houseModeDyn actIdDyn <$> distinctDyn chimsDyn
        let chimTapEvt = multicast $ latestAnyEvtWith (view _tapped) chimNodesDyn
            chimUpdEvt = latestAnyEvtWith (view _updated) chimNodesDyn

        -- add new chimney
        chimEvt <- addChimney actDyn mouseEvt
        let newChimEvt = ChimCreate <$> chimEvt
            delChimEvt = ChimDelete <$> delEvt

            newChimsEvt = multicast $ sampleDyn chimsDyn $ updateChimneys <$> (newChimEvt <|> chimUpdEvt <|> delChimEvt)

            tappedChimEvt = compact $ sampleDyn chimsDyn $ M.lookup <$> chimTapEvt

            actChimEvt = multicast $ (Just <$> tappedChimEvt) <|>
                                     (const Nothing <$> delChimEvt) <|>
                                     (const Nothing <$> roofTapEvt)

            evts = def # _chimneys .~ newChimsEvt
                       # _activeChim .~ actChimEvt
        pure { input1: newChimsEvt, input2: map (view idLens) <$> actChimEvt, output: evts }


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

rackSysForNewPanels :: Dynamic APIConfig -> Event RoofsData -> Event (Array Panel) -> Event RackingSystem
rackSysForNewPanels apiCfg roofsEvt panelsEvt = Evt.keepLatest $ performEvent $ sampleDyn apiCfg (runRackAPI <<< doRack <$> reqEvt)
    where reqEvt = sampleOn roofsEvt $ rackRequest <$> debounce (Milliseconds 100.0) panelsEvt


editHouse :: HouseConfig -> HouseEditorConf -> Node HouseTextureInfo HouseNode
editHouse houseCfg conf = do
    let house  = conf ^. _house
        hid    = house ^. idLens
        -- house is activated or not
        actDyn = conf ^. _modeDyn
        
        floor  = view _position <$> house ^. _floor

        roofsEvt = multicast $ conf ^. _roofsData
        -- house can be edited until roofplates loaded and turn in to array edit mode
        houseEditDyn = step true $ const false <$> roofsEvt
        -- build chimney mode or not
        buildChimDyn = step false $ conf ^. _buildChimney
        delChimEvt = conf ^. _delChimney

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
            roofEvts <- renderRoofs pDyn houseDyn actItemIdDyn canEditDyn (house ^. _chimneys) delChimEvt buildChimDyn

            newHouseEvt1 <- houseWithNewHeight canEditDyn houseDyn
            let newHouseEvt2 = houseWithNewSlope canEditDyn (conf ^. _slopeSelected) houseDyn actIdDyn
                newHouseEvt3 = sampleDyn houseDyn $ set _chimneys <$> (roofEvts ^. _chimney <<< _chimneys)
                newHouseEvt  = multicast $ newHouseEvt1 <|> newHouseEvt2 <|> newHouseEvt3

                roofTappedEvt = multicast $ roofEvts ^. _tapped
                chimActStEvt = multicast $ roofEvts ^. _chimney <<< _activeChim

                chimActiveEvt = filter isJust chimActStEvt
                chimTappedRoofEvt = compact $ map (view _roofId) <$> chimActStEvt

                validRoofTappedEvt = gateDyn (not <<< isActive <$> actDyn) roofTappedEvt

                actChimDyn = step Nothing chimActStEvt

                hn = def # _id           .~ (house ^. idLens)
                         # _activated    .~ (validRoofTappedEvt <|> (const hid <$> wallTap) <|> (const hid <$> chimActiveEvt))
                         # _updated      .~ (HouseOpUpdate <$> newHouseEvt)
                         # _actHouseItem .~ dynEvent (getActItem <$> actItemIdDyn <*> houseDyn <*> actChimDyn)

                -- render all roof nodes if available
                roofsDyn = step Nothing $ Just <$> roofsEvt

                rackEvt = rackSysForNewPanels (houseCfg ^. _apiConfig) roofsEvt panelsEvt
                
            nodesDyn <- localEnv (const houseCfg) $ renderRoofEditor (conf ^. _arrayEditParam) roofsDyn rackEvt

            let newPanelsEvt = multicast $ latestEvt $ allPanelsEvt <$> nodesDyn
            
            pure {input : newPanelsEvt, output : { input1: newHouseEvt, input2: Just <$> (roofTappedEvt <|> chimTappedRoofEvt), output: hn } }


wallMat :: MeshPhongMaterial
wallMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderWalls :: forall e. Polygon Vector3 -> Meter -> Node e (Event Unit)
renderWalls poly height = do
    shp <- liftEffect $ mkShapeWith $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    m <- tapMesh (def # _name       .~ "walls"
                      # _exportable .~ true) geo wallMat
    pure $ const unit <$> m ^. _tapped


renderLengths :: forall e. Maybe (List (LineSeg Vector3)) -> Node e Unit
renderLengths (Just ls) = traverse_ renderLineLength ls
renderLengths Nothing   = pure unit

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
