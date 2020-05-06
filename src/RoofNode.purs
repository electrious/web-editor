module Editor.RoofNode where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (head, init, snoc)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence_, traverse, traverse_)
import Editor.ArrayBuilder (runArrayBuilder)
import Editor.Common.Lenses (_center, _id, _mesh, _slope, _tapped, _wrapper)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.PanelLayer (createPanelLayer)
import Editor.RoofEditor (_deleteRoof, _roofVertices, createRoofEditor)
import Editor.SceneEvent (SceneTapEvent)
import Editor.WebEditor (WebEditor, _modeEvt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, create, keepLatest, subscribe, withLast)
import FRP.Event.Extra (after, multicast, performEvent)
import Math.Angle (radianVal)
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofOperation(..), RoofPlate, _azimuth, _borderPoints)
import SimplePolygon (isSimplePolygon)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (Object3D, add, matrix, mkObject3D, remove, rotateX, rotateZ, setName, setPosition, updateMatrix, updateMatrixWorld, worldToLocal)
import Three.Math.Vector (Vector2, Vector3, applyMatrix, mkVec2, mkVec3, vecX, vecY, vecZ)
import Unsafe.Coerce (unsafeCoerce)

newtype RoofNode a = RoofNode {
    roofId     :: String,
    roofUpdate :: Event RoofOperation,
    roofDelete :: Event RoofOperation,
    tapped     :: Event SceneTapEvent,
    roofObject :: Object3D a,
    disposable :: Effect Unit
}

derive instance newtypeRoofNode :: Newtype (RoofNode a) _

instance disposableRoofNode :: Disposable (RoofNode a) where
    dispose (RoofNode { disposable }) = disposable

_roofUpdate :: forall a. Lens' (RoofNode a) (Event RoofOperation)
_roofUpdate = _Newtype <<< prop (SProxy :: SProxy "roofUpdate")

_roofDelete :: forall a. Lens' (RoofNode a) (Event RoofOperation)
_roofDelete = _Newtype <<< prop (SProxy :: SProxy "roofDelete")

_roofObject :: forall a. Lens' (RoofNode a) (Object3D a)
_roofObject = _Newtype <<< prop (SProxy :: SProxy "roofObject")

-- | default material for roof plate.
defMaterial :: forall a. Material a
defMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffbb
    setTransparent true mat
    setOpacity 0.7 mat
    pure mat

-- | material for active roof plate
activeMaterial :: forall a. Material a
activeMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffff88
    setTransparent true mat
    setOpacity 0.9 mat
    pure mat

-- | material for transparent roof plate
transparentMaterial :: forall a. Material a
transparentMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffff
    setTransparent true mat
    setOpacity 0.01 mat
    pure mat

getMaterial :: forall a. Boolean -> Boolean -> Material a
getMaterial true  true  = activeMaterial
getMaterial false true  = defMaterial
getMaterial _     false = transparentMaterial

-- | create roof mesh
createRoofMesh :: forall a. Array Vector2 -> Boolean -> Boolean -> Effect (TappableMesh a)
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

mkNode :: forall a. Effect (Object3D a)
mkNode = do
    obj <- mkObject3D
    setName "roofplate" obj
    pure obj

setupRoofNode :: forall a. Object3D a -> RoofPlate -> Effect Unit
setupRoofNode obj roof = do
    let c = roof ^. _center
    setPosition (mkVec3 (vecX c) (vecY c) (vecZ c + 0.05)) obj

    -- rotate the roof node to the right azimuth and slope angles
    rotateZ (- radianVal (roof ^. _azimuth)) obj
    rotateX (- radianVal (roof ^. _slope)) obj

    -- make sure the matrix and matrixWorld are updated immediately after
    -- position and rotation changed, so that worldToLocal can use them to
    -- convert coordinates correctly
    updateMatrix obj
    updateMatrixWorld obj

-- convert all roof border points to local coordinate and get
-- only the x, y coordinates
-- NOTE: the last point will be dropped here because it's the same with the
-- first one
getBorderPoints :: forall a. Object3D a -> RoofPlate -> Effect (Array Vector2)
getBorderPoints obj roof = do 
    let toLocal p = do
            np <- worldToLocal p obj
            pure $ mkVec2 (vecX np) (vecY np)
    traverse toLocal $ fromMaybe [] (init $ roof ^. _borderPoints)

renderMesh :: forall a b. Object3D a -> { last :: Maybe (TappableMesh b), now :: TappableMesh b } -> Effect Unit
renderMesh obj {last, now} = do
    traverse_ (flip remove obj <<< view _mesh) last
    add (now ^. _mesh) obj

getNewRoof :: forall a. Object3D a -> RoofPlate -> Event (Array Vector2) -> Effect (Event RoofOperation)
getNewRoof obj roof newVertices = do
    let toParent v = applyMatrix (matrix obj) (mkVec3 (vecX v) (vecY v) 0.0)
    pure $ (RoofOpUpdate <<< flip updateRoofPlate roof <<< map toParent) <$> newVertices

-- | Create RoofNode for a RoofPlate
createRoofNode :: forall a. RoofPlate -> Array Panel -> Event Boolean -> WebEditor (RoofNode a)
createRoofNode roof panels isActive = do
    obj <- liftEffect mkNode

    modeEvt <- view _modeEvt <$> ask

    -- render panels
    panelLayer <- runArrayBuilder $ createPanelLayer panels
    liftEffect $ add (panelLayer ^. _wrapper) obj
        
    let canEditRoofEvt = (==) RoofEditing <$> modeEvt
    -- set the roof node position
    liftEffect do
        setupRoofNode obj roof
        ps <- getBorderPoints obj roof

        -- create the vertex markers editor
        let canEdit = (&&) <$> isActive <*> canEditRoofEvt
        editor <- createRoofEditor obj canEdit ps

        let vertices = (const ps <$> after 2) <|> editor ^. _roofVertices
            meshEvt = performEvent (createRoofMesh <$> vertices <*> isActive <*> canEditRoofEvt)
        
        -- add/remove mesh to the obj
        d1 <- subscribe (withLast meshEvt) (renderMesh obj)
        
        newRoof <- getNewRoof obj roof (editor ^. _roofVertices)

        -- create a stream for delete event if the current roof is not simple polygon
        { event: delEvt, push: toDel } <- create
        let delRoofEvt = delEvt <|> (const unit <$> editor ^. _deleteRoof)

        when (not $ testSimplePolygon ps) (void $ setTimeout 1000 (toDel unit))

        pure $ RoofNode {
            roofId     : roof ^. _id,
            roofDelete : multicast $ const (RoofOpDelete $ roof ^. _id) <$> delRoofEvt,
            roofUpdate : multicast newRoof,
            tapped     : multicast $ keepLatest $ view _tapped <$> meshEvt,
            roofObject : obj,
            disposable : sequence_ [d1, dispose editor]
        }