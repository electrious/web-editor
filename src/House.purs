module Editor.House where

import Prelude

import Algorithm.MeshFlatten (VertexItem, buildRTree)
import Data.Array (range)
import Data.Foldable (find, traverse_)
import Data.Traversable (traverse)
import Editor.SceneEvent (SceneMouseMoveEvent, SceneTapEvent, makeMouseMove, makeTappable)
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import RBush.RBush (RBush)
import Three.Core.Geometry (class IsGeometry, BufferGeometry, count, getAttribute, getX, getY, getZ)
import Three.Core.Material (Material, MaterialCreator, getMaterial, preload, setTransparent)
import Three.Core.Mesh (Mesh, bufferGeometry, geometry, isMesh, mkMesh)
import Three.Core.Object3D (Object3D, add, children, remove, setCastShadow, setName, setReceiveShadow)
import Three.Loader.ObjLoader (loadMTL, loadOBJ, makeMTLLoader, makeOBJLoader2, setPath)
import Three.Math.Vector (Vector3, mkVec3)
import Util (unwrap)


meshPath :: String -> Int -> String
meshPath serverUrl leadId = serverUrl <> "/leads/" <> show leadId <> "/mesh"

type HouseMesh = {
    mesh      :: Mesh Unit,
    tapped    :: Event SceneTapEvent,
    mouseMove :: Event SceneMouseMoveEvent
}

-- | create new HouseMesh, which is a mesh composed with the tap and mouse
-- events from the mesh.
mkHouseMesh :: forall a geo. IsGeometry geo => geo -> Material a -> Effect HouseMesh
mkHouseMesh geo mat = do
    mesh <- mkMesh geo mat
    setName "house-mesh" mesh

    let tapEvt = makeEvent \k -> do
            makeTappable mesh k
            pure (pure unit)
        mouseEvt = makeEvent \k -> do
            makeMouseMove mesh k
            pure (pure unit)
    
    pure {
        mesh      : mesh,
        tapped    : tapEvt,
        mouseMove : mouseEvt
    }


-- | apply the material creator's material to the mesh, which is a child of
-- the loaded Object3D
applyMaterialCreator :: forall a. MaterialCreator -> Object3D a -> Effect (Maybe HouseMesh)
applyMaterialCreator matCreator obj = do
    let mat = getMaterial "scene" matCreator
    setTransparent false mat

    let oldMesh = find isMesh (children obj)
        upd old = do
            remove old obj
            newMesh <- mkHouseMesh (geometry old) mat
            add newMesh.mesh obj
            pure newMesh
    traverse upd oldMesh


-- | load the house mesh of the specified lead
loadHouse :: String -> Int -> Effect (Event HouseMeshData)
loadHouse serverUrl leadId = do
    objLoader <- makeOBJLoader2
    mtlLoader <- makeMTLLoader

    let path = meshPath serverUrl leadId

    let setupShadow o = do
            setCastShadow true o
            setReceiveShadow true o

    pure $ unwrap $ makeEvent \k -> do
        setPath path mtlLoader
        loadMTL mtlLoader "scene.mtl" \materials -> do
            preload materials
            
            loadOBJ objLoader (path <> "scene.obj") \obj -> do
                setName "house-mesh-wrapper" obj
                traverse_ setupShadow (children obj)
                houseMesh <- applyMaterialCreator materials obj
                meshData <- traverse (getHouseMeshData obj) houseMesh
                k meshData
        pure (pure unit)


-- | house mesh data, including the mesh, the original geometry, and the
-- vertices tree built from all vertices
type HouseMeshData = {
    wrapper     :: Object3D Unit,
    mesh        :: HouseMesh,
    geometry    :: BufferGeometry,
    verticeTree :: RBush VertexItem
}

type GeometryInfo = {
    geometry :: BufferGeometry,
    vertices :: Array Vector3,
    normals  :: Array Vector3
}

-- | get the vertex position and normal vector arrays of a BufferGeometry
getGeometryInfo :: forall a. Mesh a -> GeometryInfo
getGeometryInfo mesh = let g = bufferGeometry mesh
                           posAttr = getAttribute "position" g
                           normAttr = getAttribute "normal" g

                           vecAt attr i = mkVec3 (getX i attr) (getY i attr) (getZ i attr)
                           posVecs = vecAt posAttr <$> range 0 (count posAttr - 1)
                           normVecs = vecAt normAttr <$> range 0 (count normAttr - 1)
                           in { geometry: g, vertices: posVecs, normals: normVecs }

getHouseMeshData :: Object3D Unit -> HouseMesh -> Effect HouseMeshData
getHouseMeshData obj houseMesh = do
    let d = getGeometryInfo houseMesh.mesh
    tree <- buildRTree d.vertices d.normals

    pure { wrapper: obj,
           mesh: houseMesh,
           geometry: d.geometry,
           verticeTree: tree
          }
          