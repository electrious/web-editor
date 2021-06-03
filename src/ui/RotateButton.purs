module Editor.UI.RotateButton where

import Prelude

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_name, _position, _rotation, _tapped)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Model.RotateButton (RotateButton, btnPosition, btnRotation)
import Rendering.Node (_castShadow, getEnv, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, setTransparent)

newtype RotateButtonNode = RotateButtonNode {
    tapped :: Event RotateButton
}

derive instance newtypeRotateButtonNode :: Newtype RotateButtonNode _

rotateBtnGeo :: BoxGeometry
rotateBtnGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 0.001

instance nodeRenderableRotateButton :: NodeRenderable MeshBasicMaterial RotateButton RotateButtonNode where
    render rb = do
        mat <- getEnv
        liftEffect $ setTransparent true mat
        
        btn <- tapMesh (def # _name       .~ "RotateButton"
                            # _castShadow .~ false
                            # _rotation   .~ pure (btnRotation rb)
                            # _position   .~ pure (btnPosition rb)
                       ) rotateBtnGeo mat
       
        pure $ RotateButtonNode {
            tapped : const rb <$> btn ^. _tapped
        }
