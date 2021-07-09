module Editor.UI.PlusButton where

import Prelude hiding (add)

import Custom.Mesh (mkTapDragMesh)
import Data.Function.Memoize (memoize)
import Data.Lens (Lens', view, (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_button, _dragged, _height, _orientation, _tapped, _x, _y, _z)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Math (pi)
import Math.Angle (radianVal, sin)
import Model.PlusButton (PlusButton, addDelta)
import Model.Roof.Panel (Orientation(..), validatedSlope)
import Model.RoofComponent (size)
import Rendering.Renderable (class Renderable)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, position, setCastShadow, setName, setPosition, setRenderOrder, setRotation, setScale)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3, (<+>))

newtype PlusButtonNode = PlusButtonNode {
    plusButton :: PlusButton,
    button     :: Object3D,
    tapped     :: Event PlusButton,
    dragged    :: Event (DragInfo PlusButton)
}

derive instance newtypePlusButtonNode :: Newtype PlusButtonNode _
instance isObject3DPlusButtonNode :: IsObject3D PlusButtonNode where
    toObject3D = view _button

_plusButton :: forall t a r. Newtype t { plusButton :: a | r } => Lens' t a
_plusButton = _Newtype <<< prop (Proxy :: Proxy "plusButton")

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

moveBy :: Vector3 -> PlusButtonNode -> Effect PlusButtonNode
moveBy delta node = do
    let b = node ^. _button
        pos = position b
        newPos = pos <+> delta
    
    setPosition newPos b
    pure $ node # _plusButton %~ addDelta delta

instance renderablePlusButtonNode :: Renderable e PlusButton PlusButtonNode where
    render pb = liftEffect do
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
        setTransparent true mat
        setOpacity 0.0001 mat
        
        invNode <- mkTapDragMesh (plusButtonGeo unit) mat
        setScale (mkVec3 1.5 1.0 1.0) invNode
        setCastShadow false invNode
        setRenderOrder 30 invNode
        add invNode btn

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
            plusButton : pb,
            button     : btn,
            tapped     : const pb <$> invNode ^. _tapped,
            dragged    : mkDragInfo pb <$> invNode ^. _dragged
        }