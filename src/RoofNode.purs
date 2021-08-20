module Editor.RoofNode where

import Prelude hiding (add, degree)

import Algorithm.HeatmapMesh (createNewGeometry)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Custom.Mesh (TappableMesh)
import Data.Array (head, init, last, snoc)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (sequence_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID)
import Editor.ArrayBuilder (ArrayBuilder, _editorMode, _heatmapMaterial, liftRenderingM)
import Editor.Common.Lenses (_active, _alignment, _center, _houseId, _id, _mesh, _orientation, _panelType, _polygon, _position, _roof, _rotation, _slope, _tapped)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (_heatmap)
import Editor.PanelLayer (PanelLayer, PanelLayerConfig(..), _activeArray, _currentPanels, _inactiveRoofTapped, _initPanels, _mainOrientation, _roofActive, _serverUpdated, createPanelLayer)
import Editor.PanelNode (PanelOpacity(..))
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.Rendering.PanelRendering (_opacity)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, performDynamic, step, subscribeDyn, withLast)
import FRP.Event (Event, create, keepLatest)
import FRP.Event.Extra (multicast)
import Math (pi)
import Math.Angle (degree, degreeVal, radianVal)
import Model.ActiveMode (fromBoolean)
import Model.Hardware.PanelModel (PanelModel)
import Model.Polygon (Polygon, _polyVerts, newPolygon, renderPolygon)
import Model.Racking.RoofRackingData (RoofRackingData, _arrayComps, _rafters)
import Model.Roof.Panel (Alignment(..), Orientation(..), Panel, validatedSlope)
import Model.Roof.RoofPlate (RoofOperation(..), RoofPlate, _azimuth, _borderPoints, _unifiedPoints, isFlat)
import Model.RoofSpecific (RoofSpecific, mkRoofSpecific)
import Model.ShadePoint (ShadePoint, shadePointFrom)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, mkNodeEnv, runNode)
import Rendering.Racking.RackingComponentRendering (renderRackingComp)
import Rendering.Racking.Rafter (renderRafters)
import SimplePolygon (isSimplePolygon)
import Three.Core.Geometry (ShapeGeometry, faces, vertices)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Mesh (Mesh, geometry, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, matrix, mkObject3D, remove, rotateX, rotateZ, setName, setPosition, setVisible, updateMatrix, updateMatrixWorld, worldToLocal)
import Three.Math.Vector (class Vector, Vector2, Vector3, applyMatrix, mkVec2, mkVec3, vecX, vecY, vecZ)
import Type.Proxy (Proxy(..))
import UI.RoofEditorUI (_mode)
import Util (sampleMergeDyn)


-- which env is the roof node used.
-- by default, it's used in the house editor
-- but it can also be used in 3D house builder to allow array editing directly in builder
data RoofNodeMode = RoofInEditor
                  | RoofInBuilder

derive instance genericRoofNodeMode :: Generic RoofNodeMode _
derive instance eqRoofNodeMode :: Eq RoofNodeMode


newtype RoofNodeConfig = RoofNodeConfig {
    mode            :: RoofNodeMode,
    houseId         :: Int,
    roof            :: RoofPlate,
    roofActive      :: Dynamic Boolean,
    mainOrientation :: Dynamic Orientation,
    orientation     :: Dynamic Orientation, -- current orientation of the roof
    alignment       :: Dynamic Alignment,
    panelType       :: Dynamic PanelModel,
    opacity         :: Dynamic PanelOpacity,
    racking         :: Dynamic (Maybe RoofRackingData),
    heatmap         :: Event Boolean,
    initPanels      :: Event (List Panel)
}

derive instance newtypeRoofNodeConfig :: Newtype RoofNodeConfig _
instance defaultRoofNodeConfig :: Default RoofNodeConfig where
    def = RoofNodeConfig {
        mode            : RoofInEditor,
        houseId         : 0,
        roof            : def,
        roofActive      : pure false,
        mainOrientation : pure Landscape,
        orientation     : pure Landscape,
        alignment       : pure Grid,
        panelType       : pure def,
        opacity         : pure Opaque,
        racking         : pure Nothing,
        heatmap         : empty,
        initPanels      : empty
    }

_racking :: forall t a r. Newtype t { racking :: a | r } => Lens' t a
_racking = _Newtype <<< prop (Proxy :: Proxy "racking")

newtype RoofNode = RoofNode {
    roofId        :: UUID,
    roof          :: RoofPlate,
    -- roof plate editing operation events
    updated       :: Event RoofOperation,
    deleted       :: Event RoofOperation,
    tapped        :: Event RoofPlate,

    roofObject    :: Object3D,

    -- array editor events
    currentPanels :: Event (List Panel),
    serverUpdated :: Event Unit,
    alignment     :: Event (Maybe (RoofSpecific Alignment)),
    orientation   :: Event (Maybe (RoofSpecific Orientation)),

    disposable    :: Effect Unit
}

derive instance newtypeRoofNode :: Newtype RoofNode _
instance isObject3DRoofNode :: IsObject3D RoofNode where
    toObject3D = view _roofObject
instance disposableRoofNode :: Disposable RoofNode where
    dispose (RoofNode { disposable }) = disposable

_roofObject :: Lens' RoofNode Object3D
_roofObject = _Newtype <<< prop (Proxy :: Proxy "roofObject")

-- | default material for roof plate.
defMaterial :: MeshBasicMaterial
defMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffbb
    setTransparent true mat
    setOpacity 0.7 mat
    pure mat

-- | material for active roof plate
activeMaterial :: MeshBasicMaterial
activeMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffff88
    setTransparent true mat
    setOpacity 0.9 mat
    pure mat

-- | material for transparent roof plate
transparentMaterial :: MeshBasicMaterial
transparentMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffff
    setTransparent true mat
    setOpacity 0.01 mat
    pure mat

getMaterial :: Boolean -> Boolean -> MeshBasicMaterial
getMaterial true  true  = activeMaterial
getMaterial false true  = defMaterial
getMaterial _     false = transparentMaterial

-- | create roof mesh
createRoofMesh :: forall v. Vector v => Polygon v -> Boolean -> Boolean -> Effect TappableMesh
createRoofMesh poly active canEditRoof = do
    let mat = getMaterial active canEditRoof
    m <- renderPolygon poly mat
    setName "roof-mesh" m
    pure m

updateRoofPlate :: Array Vector3 -> RoofPlate -> RoofPlate
updateRoofPlate [] roof = roof
updateRoofPlate ps roof = roof # _borderPoints .~ newPs
    where newPs = fromMaybe (roof ^. _borderPoints) $ snoc ps <$> head ps


-- test if a polygon is simple or with self-intersections
testSimplePolygon :: forall v. Vector v => Polygon v -> Boolean
testSimplePolygon poly = isSimplePolygon (f <$> poly ^. _polyVerts)
    where f p = [vecX p, vecY p]

mkNode :: String -> Effect Object3D
mkNode name = do
    obj <- mkObject3D
    setName name obj
    pure obj

setupRoofNode :: forall a b. IsObject3D a => IsObject3D b => a -> b -> RoofPlate -> Effect Unit
setupRoofNode obj content roof = do
    let c = roof ^. _center
    setPosition (mkVec3 (vecX c) (vecY c) (vecZ c + 0.05)) obj

    -- rotate the roof node to the right azimuth and slope angles
    rotateZ (- radianVal (roof ^. _azimuth)) obj
    rotateX (- radianVal (roof ^. _slope)) obj
    rotateZ pi obj

    -- rotate the content node if rotateOverride is not 0
    let rot = roof ^. _rotation
    when (degreeVal rot /= 0.0) (rotateZ (radianVal rot) content)

    -- make sure the matrix and matrixWorld are updated immediately after
    -- position and rotation changed, so that worldToLocal can use them to
    -- convert coordinates correctly
    updateMatrix obj
    updateMatrixWorld obj

-- convert all roof border points to local coordinate and get
-- only the x, y coordinates
-- NOTE: the last point will be dropped here because it's the same with the
-- first one
getBorderPolygon :: forall a. IsObject3D a => a -> RoofPlate -> Effect (Polygon Vector2)
getBorderPolygon obj roof = map newPolygon $ traverse toLocal $ delRedundant $ roof ^. _borderPoints
    where toLocal p = do
              np <- worldToLocal p obj
              pure $ mkVec2 (vecX np) (vecY np)

          delRedundant vs = if head vs == last vs
                            then fromMaybe vs $ init vs
                            else vs

-- convert all unifiedPoints to local coordinate and create ShadePoint values from them
getShadePoints :: forall a. IsObject3D a => a -> RoofPlate -> Effect (Array ShadePoint)
getShadePoints obj roof = traverse mkShade $ fromMaybe [] (roof ^. _unifiedPoints)
    where mkShade up = do
              let p = shadePointFrom up
              np <- worldToLocal (p ^. _position) obj
              pure $ p # _position .~ np

renderMesh :: forall a b. IsObject3D a => IsObject3D b => a -> { last :: Maybe b, now :: b } -> Effect Unit
renderMesh obj {last, now} = do
    traverse_ (flip remove obj) last
    add now obj

getNewRoof :: forall a v. Vector v => IsObject3D a => a -> RoofPlate -> Event (Polygon v) -> Effect (Event RoofOperation)
getNewRoof obj roof polyEvt = do
    let toParent v = applyMatrix (matrix obj) (mkVec3 (vecX v) (vecY v) 0.0)
    pure $ (RoofOpUpdate <<< flip updateRoofPlate roof <<< map toParent <<< view _polyVerts) <$> polyEvt

renderPanels :: forall a. IsObject3D a => RoofNodeConfig -> a -> ArrayBuilder (Tuple PanelLayer (Effect Unit))
renderPanels cfg content = do
    l <- createPanelLayer (PanelLayerConfig {
                                houseId         : cfg ^. _houseId,
                                roof            : cfg ^. _roof,
                                roofActive      : cfg ^. _roofActive,
                                mainOrientation : cfg ^. _mainOrientation,
                                orientation     : cfg ^. _orientation,
                                alignment       : cfg ^. _alignment,
                                panelType       : cfg ^. _panelType,
                                initPanels      : cfg ^. _initPanels,
                                opacity         : cfg ^. _opacity
                            })

    modeDyn <- view _editorMode <$> ask

    -- add panel layer dynamically only in array editing mode
    d <- liftEffect $ subscribeDyn modeDyn \m -> do
             if m == ArrayEditing || m == Showing
                 then add l content
                 else remove l content
    pure $ Tuple l d

evtInMaybe :: forall a b. (a -> Event b) -> Maybe a -> Event b
evtInMaybe _ Nothing  = empty
evtInMaybe f (Just a) = f a


renderRacking :: forall e. RoofPlate -> List Panel -> Maybe RoofRackingData -> Node e Unit
renderRacking _ _     Nothing  = pure unit
renderRacking roof ps (Just r) = do
    renderRafters $ r ^. _rafters
    let defSlope = degree 0.0
        slope = if isFlat roof
                then fromMaybe defSlope $ join $ validatedSlope <$> L.head ps
                else defSlope
    traverse_ (renderRackingComp slope) $ r ^. _arrayComps

createHeatmapMesh :: RoofPlate -> Object3D -> MeshBasicMaterial -> TappableMesh -> Effect Mesh
createHeatmapMesh roof roofNode mat m = do
    let geo :: ShapeGeometry
        geo     = geometry (m ^. _mesh)
        verts   = vertices geo
        tris    = faces geo
    
    shadePs <- getShadePoints roofNode roof
    newGeo <- createNewGeometry verts tris shadePs

    mesh <- mkMesh newGeo mat
    setVisible false mesh
    pure mesh

canShowHeatmap :: EditorMode -> Boolean -> Boolean
canShowHeatmap ArrayEditing s = s
canShowHeatmap _ _            = false

-- | Create RoofNode for a RoofPlate
createRoofNode :: RoofNodeConfig -> ArrayBuilder RoofNode
createRoofNode cfg = do
    obj     <- liftEffect $ mkNode "roofplate"
    content <- liftEffect $ mkNode "roof-content"
    liftEffect $ add content obj

    modeDyn <- view _editorMode <$> ask
    hmMat   <- view _heatmapMaterial <$> liftRenderingM ask

    let roof = cfg ^. _roof
        nodeEnv = mkNodeEnv obj unit

    -- render panels
    Tuple panelLayer d00 <- renderPanels cfg content

    let panelsEvt = multicast $ panelLayer ^. _currentPanels
        panelsDyn = step Nil panelsEvt
    -- render racking components
    Tuple _ d01 <- liftEffect $ flip runNode nodeEnv $ dynamic_ $ sampleMergeDyn (renderRacking roof) panelsDyn (cfg ^. _racking)
    
    let mode = cfg ^. _mode
        -- get the panel tap event on inactive roofs
        roofTapOnPanelEvt = panelLayer ^. _inactiveRoofTapped

        -- roof only editable in Editor mode, not in Builder mode
        canEditRoofDyn = if mode == RoofInEditor
                         then (==) RoofEditing <$> modeDyn
                         else pure false
        
        rid            = roof ^. _id
        isActive       = cfg ^. _roofActive

    -- set the roof node position
    liftEffect do
        setupRoofNode obj content roof
        poly <- getBorderPolygon obj roof

        -- create the vertex markers editor
        let canEdit = (&&) <$> isActive <*> canEditRoofDyn
            -- PolyEditorConf
            opt = def # _active   .~ (fromBoolean <$> canEdit)
                      # _polygon  .~ poly
        Tuple editor d0 <- runNode (createPolyEditor opt) nodeEnv

        let polyDyn = step poly (editor ^. _polygon)
            meshDyn = performDynamic (createRoofMesh <$> polyDyn <*> isActive <*> canEditRoofDyn)
            roofTapEvt = const unit <$> keepLatest (view _tapped <$> dynEvent meshDyn)

        -- create heatmap mesh and add to roofnode
        hmMesh <- createHeatmapMesh roof obj hmMat =<< current meshDyn
        add hmMesh obj

        -- add/remove mesh to the obj
        d1 <- subscribeDyn (withLast meshDyn) (renderMesh obj)

        let canShowHeatmapDyn = canShowHeatmap <$> modeDyn <*> (step false $ cfg ^. _heatmap)
        d2 <- subscribeDyn canShowHeatmapDyn (flip setVisible hmMesh)

        newRoof <- getNewRoof obj roof (editor ^. _polygon)

        -- create a stream for delete event if the current roof is not simple polygon
        { event: delEvt, push: toDel } <- create
        let delRoofEvt = delEvt <|> (const unit <$> editor ^. _delete)

        when (not $ testSimplePolygon poly) (void $ setTimeout 1000 (toDel unit))

        let actArrEvt = panelLayer ^. _activeArray
            roofSpecActArrEvt = multicast $ map (mkRoofSpecific rid) <$> actArrEvt

        pure $ RoofNode {
            roofId        : rid,
            roof          : roof,
            deleted       : multicast $ const (RoofOpDelete rid) <$> delRoofEvt,
            updated       : multicast newRoof,
            tapped        : multicast $ const roof <$> (roofTapEvt <|> roofTapOnPanelEvt),
            roofObject    : obj,
            disposable    : sequence_ [d00, dispose d01, dispose d0, d1, d2],
            currentPanels : panelsEvt,
            serverUpdated : panelLayer ^. _serverUpdated,
            alignment     : map (map (view _alignment)) <$> roofSpecActArrEvt,
            orientation   : map (map (view _orientation)) <$> roofSpecActArrEvt
        }
