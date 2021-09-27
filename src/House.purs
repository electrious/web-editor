module Editor.House where

import Prelude hiding (add)

import Algorithm.MeshFlatten (VertexItem, buildRTree)
import Taihe.Mesh (TapMouseMesh(..), mouseEvtOn, tapEvtOn)
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
import Three.Core.Geometry (BufferGeometry, computeVertexNormals, count, getAttribute, getX, getY, getZ)
import Three.Core.Material (preload, setTransparent)
import Three.Core.Mesh (Mesh, geometry, isMesh, material)
import Three.Core.Object3D (Object3D, children, setCastShadow, setName, setReceiveShadow)
import Three.Loader.ObjLoader (loadMTL, loadOBJ, makeMTLLoader, makeOBJLoader, setMaterials, setPath)
import Three.Math.Vector (Vector3, mkVec3)


meshPath :: String -> Int -> String
meshPath serverUrl leadId = serverUrl <> "/leads/" <> show leadId <> "/mesh/"

type HouseMesh = TapMouseMesh

-- | create new HouseMesh, which is a mesh composed with the tap and mouse
-- events from the mesh.
mkHouseMesh :: Mesh -> Effect HouseMesh
mkHouseMesh m = do
    let mesh = TapMouseMesh {
            mesh      : m,
            tapped    : tapEvtOn m,
            mouseMove : mouseEvtOn m
            }
    setName "house-mesh" mesh
    pure mesh


getHouseMesh :: Object3D -> Effect (Maybe HouseMesh)
getHouseMesh obj = traverse mkHouseMesh $ find isMesh (children obj)


-- | load the house mesh of the specified lead
loadHouseModel :: String -> Int -> Effect (Event HouseMeshData)
loadHouseModel serverUrl leadId = do
    objLoader <- makeOBJLoader
    mtlLoader <- makeMTLLoader

    let path = meshPath serverUrl leadId

        setupMeshProp :: Mesh -> Effect Unit
        setupMeshProp o = do
            setCastShadow true o
            setReceiveShadow true o
            setTransparent false $ material o
            computeVertexNormals $ (geometry o :: BufferGeometry)

    pure $ compact $ makeEvent \k -> do
        setPath path mtlLoader
        loadMTL mtlLoader "scene.mtl" \materials -> do
            preload materials

            -- set the OBJloader to use the right materials
            setMaterials materials objLoader
            
            loadOBJ objLoader (path <> "scene.obj") \obj -> do
                setName "house-mesh-wrapper" obj
                traverse_ setupMeshProp (children obj)
                houseMesh <- getHouseMesh obj
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
getGeometryInfo mesh = let g = geometry mesh
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
