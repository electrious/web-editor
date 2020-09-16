module Editor.UI.RotateButton where

import Prelude

import Control.Monad.Reader (ask)
import Custom.Mesh (mkTappableMesh)
import Data.Function.Memoize (memoize)
import Data.Lens (view, (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_button, _height, _mesh, _tapped, _x, _y, _z)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Math.Angle (radianVal, sin)
import Model.Roof.Panel (validatedSlope)
import Model.RoofComponent (size)
import Model.RotateButton (RotateButton)
import Rendering.Renderable (class Renderable, _rotateButtonTexture)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Core.Mesh (Mesh)
import Three.Core.Object3D (class IsObject3D, setCastShadow, setName, setPosition, setRotation, toObject3D)
import Three.Loader.TextureLoader (Texture, loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

newtype RotateButtonNode = RotateButtonNode {
    button :: Mesh,
    tapped :: Event RotateButton
}

derive instance newtypeRotateButtonNode :: Newtype RotateButtonNode _
instance isObject3DRotateButtonNode :: IsObject3D RotateButtonNode where
    toObject3D = toObject3D <<< view _button

rotateBtnGeo :: Unit -> BoxGeometry
rotateBtnGeo = memoize $ const $ unsafePerformEffect $ mkBoxGeometry 1.0 1.0 0.001

rotateBtnTexture :: String -> Texture
rotateBtnTexture = memoize \imgPath -> unsafePerformEffect do
    loader <- mkTextureLoader
    loadTexture imgPath loader


instance renderableRotateButton :: Renderable RotateButton RotateButtonNode where
    render rb = do
        texture <- view _rotateButtonTexture <$> ask
        mat <- liftEffect $ mkMeshBasicMaterialWithTexture texture
        btn <- liftEffect $ mkTappableMesh (rotateBtnGeo unit) mat
        
        liftEffect $ setName "RotateButton" btn

        liftEffect $ setCastShadow false btn

        let x = meterVal $ rb ^. _x
            y = meterVal $ rb ^. _y
            z = meterVal $ rb ^. _z
        
        case validatedSlope rb of
            Just slope -> liftEffect do
                setRotation (mkEuler (radianVal slope) 0.0 0.0) btn
                let h = meterVal ((size rb) ^. _height) * 0.5 * sin slope
                setPosition (mkVec3 x y (z + h)) btn
            Nothing -> liftEffect $ setPosition (mkVec3 x y z) btn

        pure $ RotateButtonNode {
            button : btn ^. _mesh,
            tapped : const rb <$> btn ^. _tapped
        }
