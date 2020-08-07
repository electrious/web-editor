module Editor.UI.PlusButton where

import Prelude

import Data.Function.Memoize (memoize)
import Data.Newtype (class Newtype)
import Editor.UI.DragInfo (DragInfo)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Model.PlusButton (PlusButton)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Object3D (Object3D, mkObject3D, setName)

newtype PlusButtonNode = PlusButtonNode {
    button  :: Object3D,
    tapped  :: Event PlusButton,
    dragged :: Event (DragInfo PlusButton)
}

derive instance newtypePlusButtonNode :: Newtype PlusButtonNode _

plusButtonGeo :: Unit -> BoxGeometry
plusButtonGeo = memoize $ const $ unsafePerformEffect $ mkBoxGeometry 1.0 1.0 0.001

mkPlusButtonNode :: PlusButton -> Effect PlusButtonNode
mkPlusButtonNode pb = do
    btn <- mkObject3D
    setName "PlusButton" btn
