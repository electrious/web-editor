module Editor.House where

import Prelude hiding (add)

import Algorithm.MeshFlatten (VertexItem, buildRTree)
import Custom.Mesh (TapMouseMesh, mkTapMouseMesh)
import Data.Array (range)
import Data.Compactable (compact)
import Data.Foldable (find, traverse_)
import Data.Lens ((^.))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_mesh)
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import RBush.RBush (RBush)
import Three.Core.Geometry (class IsGeometry, BufferGeometry, Geometry, count, getAttribute, getX, getY, getZ)
import Three.Core.Material (class IsMaterial, MaterialCreator, MeshBasicMaterial, getMaterial, preload, setTransparent)
import Three.Core.Mesh (Mesh, bufferGeometry, geometry, isMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, children, remove, setCastShadow, setName, setReceiveShadow)
import Three.Loader.ObjLoader (loadMTL, loadOBJ, makeMTLLoader, makeOBJLoader2, setPath)
import Three.Math.Vector (Vector3, mkVec3)


meshPath :: String -> Int -> String
meshPath serverUrl leadId = serverUrl <> "/leads/" <> show leadId <> "/mesh/"

type HouseMesh = TapMouseMesh

-- | create new HouseMesh, which is a mesh composed with the tap and mouse
-- events from the mesh.
mkHouseMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect HouseMesh
mkHouseMesh geo mat = do
    mesh <- mkTapMouseMesh geo mat
    setName "house-mesh" mesh
    pure mesh


-- | apply the material creator's material to the mesh, which is a child of
-- the loaded Object3D
applyMaterialCreator :: forall a. IsObject3D a => MaterialCreator -> a -> Effect (Maybe HouseMesh)
applyMaterialCreator matCreator obj = do
    let mat :: MeshBasicMaterial
        mat = getMaterial "scene" matCreator
    setTransparent false mat

    let oldMesh = find isMesh (children obj)
        upd old = do
            remove old obj
            let geo :: Geometry
                geo = geometry old
            newMesh <- mkHouseMesh geo mat
            add newMesh obj
            pure newMesh
    traverse upd oldMesh


-- | load the house mesh of the specified lead
loadHouseModel :: String -> Int -> Effect (Event HouseMeshData)
loadHouseModel serverUrl leadId = do
    objLoader <- makeOBJLoader2
    mtlLoader <- makeMTLLoader

    let path = meshPath serverUrl leadId

    let setupShadow :: Object3D -> Effect Unit
        setupShadow o = do
            setCastShadow true o
            setReceiveShadow true o

    pure $ compact $ makeEvent \k -> do
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
newtype HouseMeshData = HouseMeshData {
    wrapper     :: Object3D,
    mesh        :: HouseMesh,
    geometry    :: BufferGeometry,
    verticeTree :: RBush VertexItem
}

derive instance newtypeHouseMeshData :: Newtype HouseMeshData _

type GeometryInfo = {
    geometry :: BufferGeometry,
    vertices :: Array Vector3,
    normals  :: Array Vector3
}

-- | get the vertex position and normal vector arrays of a BufferGeometry
getGeometryInfo :: Mesh -> GeometryInfo
getGeometryInfo mesh = let g = bufferGeometry mesh
                           posAttr = getAttribute "position" g
                           normAttr = getAttribute "normal" g

                           vecAt attr i = mkVec3 (getX i attr) (getY i attr) (getZ i attr)
                           posVecs = vecAt posAttr <$> range 0 (count posAttr - 1)
                           normVecs = vecAt normAttr <$> range 0 (count normAttr - 1)
                           in { geometry: g, vertices: posVecs, normals: normVecs }

getHouseMeshData :: Object3D -> HouseMesh -> Effect HouseMeshData
getHouseMeshData obj houseMesh = do
    let d = getGeometryInfo $ houseMesh ^. _mesh
    tree <- buildRTree d.vertices d.normals

    pure $ HouseMeshData {
            wrapper     : obj,
            mesh        : houseMesh,
            geometry    : d.geometry,
            verticeTree : tree
        }
