module Editor.UI.PlusButton where

import Prelude hiding (add)

import Custom.Mesh (mkTapDragMesh)
import Data.Function.Memoize (memoize)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_dragged, _height, _mesh, _orientation, _tapped, _x, _y, _z)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Math (pi)
import Math.Angle (radianVal, sin)
import Model.PlusButton (PlusButton)
import Model.Roof.Panel (Orientation(..), validatedSlope)
import Model.RoofComponent (size)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setCastShadow, setName, setPosition, setRenderOrder, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3)

newtype PlusButtonNode = PlusButtonNode {
    button  :: Object3D,
    tapped  :: Event PlusButton,
    dragged :: Event (DragInfo PlusButton)
}

derive instance newtypePlusButtonNode :: Newtype PlusButtonNode _

plusButtonGeo :: Unit -> BoxGeometry
plusButtonGeo = memoize $ const $ unsafePerformEffect $ mkBoxGeometry 1.0 1.0 0.001

whiteMaterial :: Unit -> MeshBasicMaterial
whiteMaterial = memoize $ const $ unsafePerformEffect $ mkMeshBasicMaterial 0xffffff

mkPlusBtnPart :: Vector3 -> Vector3 -> Effect Mesh
mkPlusBtnPart scale pos = do
    m <- mkMesh (plusButtonGeo unit) (whiteMaterial unit)
    setScale scale m
    setPosition pos m
    setCastShadow false m
    pure m

mkPlusButtonNode :: PlusButton -> Effect PlusButtonNode
mkPlusButtonNode pb = do
    btn <- mkObject3D
    setName "PlusButton" btn

    let addToBtn = flip add btn

    -- setup left, right, top and bottom parts
    mkPlusBtnPart (mkVec3 0.05 1.0 1.0) (mkVec3 (-0.75) 0.0 0.0)  >>= addToBtn
    mkPlusBtnPart (mkVec3 0.05 1.0 1.0) (mkVec3 0.75 0.0 0.0)     >>= addToBtn
    mkPlusBtnPart (mkVec3 1.5 0.05 1.0) (mkVec3 0.0 0.475 0.0)    >>= addToBtn
    mkPlusBtnPart (mkVec3 1.5 0.05 1.0) (mkVec3 0.0 (-0.475) 0.0) >>= addToBtn

    -- setup the invisible mesh for tap and drag
    mat <- mkMeshBasicMaterial 0xffffff
    setOpacity 0.0001 mat
    
    invNode <- mkTapDragMesh (plusButtonGeo unit) mat
    let m = invNode ^. _mesh
    setScale (mkVec3 1.5 1.0 1.0) m
    setCastShadow false m
    setRenderOrder 30 m
    addToBtn m

    -- set rotation and position of the button
    case validatedSlope pb of
        Just slope -> do
            if pb ^. _orientation == Portrait
              then setRotation (mkEuler 0.0 (- radianVal slope) (pi / 2.0)) btn
              else setRotation (mkEuler (radianVal slope) 0.0 0.0) btn
            let h = meterVal (size pb ^. _height) * 0.5 * sin slope
            setPosition (mkVec3 (meterVal $ pb ^. _x) (meterVal $ pb ^. _y) (meterVal (pb ^. _z) + h)) btn
        Nothing -> do
            if pb ^. _orientation == Portrait
              then setRotation (mkEuler 0.0 0.0 (pi / 2.0)) btn
              else pure unit
            setPosition (mkVec3 (meterVal $ pb ^. _x) (meterVal $ pb ^. _y) (meterVal $ pb ^. _z)) btn
    
    pure $ PlusButtonNode {
        button  : btn,
        tapped  : const pb <$> invNode ^. _tapped,
        dragged : mkDragInfo pb <$> invNode ^. _dragged
    }