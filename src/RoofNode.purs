module Editor.RoofNode where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (head, init, snoc)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence_, traverse, traverse_)
import Editor.Disposable (class Disposable, dispose)
import Editor.RoofEditor (_deleteRoof, _roofVertices, createRoofEditor)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, create, keepLatest, subscribe, withLast)
import FRP.Event.Extra (multicast, performEvent)
import Math.Angle (radianVal)
import Models.Roof.RoofPlate (RoofOperation(..), RoofPlate, _azimuth, _borderPoints, _center, _slope)
import Models.Roof.RoofPlate as RP
import SimplePolygon (isSimplePolygon)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Mesh (setMaterial)
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

_roofId :: forall a. Lens' (RoofNode a) String
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofId")

_roofUpdate :: forall a. Lens' (RoofNode a) (Event RoofOperation)
_roofUpdate = _Newtype <<< prop (SProxy :: SProxy "roofUpdate")

_roofDelete :: forall a. Lens' (RoofNode a) (Event RoofOperation)
_roofDelete = _Newtype <<< prop (SProxy :: SProxy "roofDelete")

_tapped :: forall a. Lens' (RoofNode a) (Event SceneTapEvent)
_tapped = _Newtype <<< prop (SProxy :: SProxy "tapped")

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

getMaterial :: forall a. Boolean -> Material a
getMaterial true = activeMaterial
getMaterial false = defMaterial

-- | create roof mesh
createRoofMesh :: forall a. Array Vector2 -> Boolean -> Effect (TappableMesh a)
createRoofMesh ps active = do
    shp <- mkShape ps
    geo <- mkShapeGeometry shp
    m <- mkTappableMesh geo (getMaterial active)
    setName "roof-mesh" m.mesh
    pure m

updateRoofPlate :: Array Vector3 -> RoofPlate -> RoofPlate
updateRoofPlate [] roof = roof
updateRoofPlate ps roof = roof # _borderPoints .~ newPs
    where newPs = fromMaybe (roof ^. _borderPoints) $ snoc ps <$> head ps


-- test if a polygon is simple or with self-intersections
testSimplePolygon :: Array Vector2 -> Boolean
testSimplePolygon ps = isSimplePolygon (f <$> ps)
    where f p = [vecX p, vecY p]


-- | Create RoofNode for a RoofPlate
createRoofNode :: forall a. RoofPlate -> Event Boolean -> Effect (RoofNode a)
createRoofNode roof isActive = do
    obj <- mkObject3D
    setName "roofplate" obj

    -- set the roof node position
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
    let toLocal p = do
            np <- worldToLocal p obj
            pure $ mkVec2 (vecX np) (vecY np)
    ps <- traverse toLocal $ fromMaybe [] (init $ roof ^. _borderPoints)

    { event: defVerts, push: setDefVerts } <- create

    -- create the vertex markers editor
    editor <- createRoofEditor obj isActive ps

    let vertices = defVerts <|> editor ^. _roofVertices
        meshEvt = performEvent (lift2 createRoofMesh vertices isActive)
    
    -- add/remove mesh to the obj
    d1 <- subscribe (withLast meshEvt) \{last, now} -> do
              traverse_ (\o -> remove o.mesh obj) last
              add now.mesh obj
    
    -- set mesh material based on activity state
    let e = performEvent $ lift2 (\m a -> setMaterial (getMaterial a) m.mesh) meshEvt isActive
    d2 <- subscribe e (\_ -> pure init)

    let tapped = keepLatest $ _.tapped <$> meshEvt

        toParent v = applyMatrix (matrix obj) (mkVec3 (vecX v) (vecY v) 0.0)
        
        newRoofs = (RoofOpUpdate <<< flip updateRoofPlate roof <<< map toParent) <$> editor ^. _roofVertices
    
    -- create a stream for delete event if the current roof is not simple polygon
    { event: delEvt, push: toDel } <- create
    let delRoofEvt = delEvt <|> (const unit <$> editor ^. _deleteRoof)

    -- set default vertices
    setDefVerts ps

    when (not $ testSimplePolygon ps) (void $ setTimeout 1000 (toDel unit))

    pure $ RoofNode {
        roofId     : roof ^. RP._roofId,
        roofDelete : multicast $ const (RoofOpDelete $ roof ^. RP._roofId) <$> delRoofEvt,
        roofUpdate : multicast newRoofs,
        tapped     : multicast tapped,
        roofObject : obj,
        disposable : sequence_ [d1, d2, dispose editor]
    }