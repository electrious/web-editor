module Editor.RoofNode where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (head, init, snoc)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence_, traverse, traverse_)
import Data.UUID (UUID)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_center, _id, _mesh, _modeDyn, _slope, _tapped, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (HouseEditor, performEditorDyn)
import Editor.PanelLayer (createPanelLayer)
import Editor.RoofEditor (_deleteRoof, _roofVertices, createRoofEditor)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, dynEvent, performDynamic, step, subscribeDyn, withLast)
import FRP.Event (Event, create, keepLatest)
import FRP.Event.Extra (multicast)
import Math (pi)
import Math.Angle (degreeVal, radianVal)
import Model.Racking.RackingType (RackingType)
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofOperation(..), RoofPlate, _azimuth, _borderPoints, _rotation)
import SimplePolygon (isSimplePolygon)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (class IsObject3D, Object3D, add, matrix, mkObject3D, remove, rotateX, rotateZ, setName, setPosition, updateMatrix, updateMatrixWorld, worldToLocal)
import Three.Math.Vector (Vector2, Vector3, applyMatrix, mkVec2, mkVec3, vecX, vecY, vecZ)
import Unsafe.Coerce (unsafeCoerce)

newtype RoofNode = RoofNode {
    roofId     :: UUID,
    roofUpdate :: Event RoofOperation,
    roofDelete :: Event RoofOperation,
    tapped     :: Event SceneTapEvent,
    roofObject :: Object3D,
    disposable :: Effect Unit
}

derive instance newtypeRoofNode :: Newtype RoofNode _

instance disposableRoofNode :: Disposable RoofNode where
    dispose (RoofNode { disposable }) = disposable

_roofUpdate :: Lens' RoofNode (Event RoofOperation)
_roofUpdate = _Newtype <<< prop (SProxy :: SProxy "roofUpdate")

_roofDelete :: Lens' RoofNode (Event RoofOperation)
_roofDelete = _Newtype <<< prop (SProxy :: SProxy "roofDelete")

_roofObject :: Lens' RoofNode Object3D
_roofObject = _Newtype <<< prop (SProxy :: SProxy "roofObject")

-- | default material for roof plate.
defMaterial :: MeshBasicMaterial
defMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffbb
    setTransparent true mat
    setOpacity 0.7 mat
    pure mat

-- | material for active roof plate
activeMaterial :: MeshBasicMaterial
activeMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffff88
    setTransparent true mat
    setOpacity 0.9 mat
    pure mat

-- | material for transparent roof plate
transparentMaterial :: MeshBasicMaterial
transparentMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffff
    setTransparent true mat
    setOpacity 0.01 mat
    pure mat

getMaterial :: Boolean -> Boolean -> MeshBasicMaterial
getMaterial true  true  = activeMaterial
getMaterial false true  = defMaterial
getMaterial _     false = transparentMaterial

-- | create roof mesh
createRoofMesh :: Array Vector2 -> Boolean -> Boolean -> Effect TappableMesh
createRoofMesh ps active canEditRoof = do
    shp <- mkShape ps
    geo <- mkShapeGeometry shp
    m <- mkTappableMesh geo (getMaterial active canEditRoof)
    setName "roof-mesh" $ m ^. _mesh
    pure m

updateRoofPlate :: Array Vector3 -> RoofPlate -> RoofPlate
updateRoofPlate [] roof = roof
updateRoofPlate ps roof = roof # _borderPoints .~ newPs
    where newPs = fromMaybe (roof ^. _borderPoints) $ snoc ps <$> head ps


-- test if a polygon is simple or with self-intersections
testSimplePolygon :: Array Vector2 -> Boolean
testSimplePolygon ps = isSimplePolygon (f <$> ps)
    where f p = [vecX p, vecY p]

mkNode :: String -> Effect Object3D
mkNode name  = do
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
getBorderPoints :: forall a. IsObject3D a => a -> RoofPlate -> Effect (Array Vector2)
getBorderPoints obj roof = traverse toLocal $ fromMaybe [] (init $ roof ^. _borderPoints)
    where toLocal p = do
              np <- worldToLocal p obj
              pure $ mkVec2 (vecX np) (vecY np)

renderMesh :: forall a. IsObject3D a => a -> { last :: Maybe TappableMesh, now :: TappableMesh } -> Effect Unit
renderMesh obj {last, now} = do
    traverse_ (flip remove obj <<< view _mesh) last
    add (now ^. _mesh) obj

getNewRoof :: forall a. IsObject3D a => a -> RoofPlate -> Event (Array Vector2) -> Effect (Event RoofOperation)
getNewRoof obj roof newVertices = do
    let toParent v = applyMatrix (matrix obj) (mkVec3 (vecX v) (vecY v) 0.0)
    pure $ (RoofOpUpdate <<< flip updateRoofPlate roof <<< map toParent) <$> newVertices

renderPanels :: forall a. IsObject3D a => a -> RackingType -> Array Panel -> Dynamic EditorMode -> HouseEditor (Effect Unit)
renderPanels content rackType panels modeDyn = do
    -- don't build the panel layer when it's roof editing mode
    let builder RoofEditing = pure Nothing
        builder _ = Just <$> runArrayBuilder rackType (createPanelLayer panels)

        render { last, now } = do
            traverse_ (flip remove content <<< view _wrapper) $ join last
            traverse_ (flip add content <<< view _wrapper) now
    panelLayerDyn <- performEditorDyn $ builder <$> modeDyn
    liftEffect $ subscribeDyn (withLast panelLayerDyn) render

-- | Create RoofNode for a RoofPlate
createRoofNode :: RoofPlate -> RackingType -> Array Panel -> Dynamic Boolean -> HouseEditor RoofNode
createRoofNode roof rackType panels isActive = do
    obj <- liftEffect $ mkNode "roofplate"
    content <- liftEffect $ mkNode "roof-content"
    liftEffect $ add content obj

    modeDyn <- view _modeDyn <$> ask

    -- render panels
    d <- renderPanels content rackType panels modeDyn
    
    let canEditRoofDyn = (==) RoofEditing <$> modeDyn
    -- set the roof node position
    liftEffect do
        setupRoofNode obj content roof
        ps <- getBorderPoints obj roof

        -- create the vertex markers editor
        let canEdit = (&&) <$> isActive <*> canEditRoofDyn
        editor <- createRoofEditor obj canEdit ps

        let vertices = step ps (editor ^. _roofVertices)
            meshDyn = performDynamic (createRoofMesh <$> vertices <*> isActive <*> canEditRoofDyn)
        
        -- add/remove mesh to the obj
        d1 <- subscribeDyn (withLast meshDyn) (renderMesh obj)
        
        newRoof <- getNewRoof obj roof (editor ^. _roofVertices)

        -- create a stream for delete event if the current roof is not simple polygon
        { event: delEvt, push: toDel } <- create
        let delRoofEvt = delEvt <|> (const unit <$> editor ^. _deleteRoof)

        when (not $ testSimplePolygon ps) (void $ setTimeout 1000 (toDel unit))

        pure $ RoofNode {
            roofId     : roof ^. _id,
            roofDelete : multicast $ const (RoofOpDelete $ roof ^. _id) <$> delRoofEvt,
            roofUpdate : multicast newRoof,
            tapped     : multicast $ keepLatest $ view _tapped <$> (dynEvent meshDyn),
            roofObject : obj,
            disposable : sequence_ [d, d1, dispose editor]
        }