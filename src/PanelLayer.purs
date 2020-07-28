module Editor.PanelLayer where

import Prelude hiding (add)

import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_)
import Editor.ArrayBuilder (ArrayBuilder)
import Editor.PanelArrayLayout (PanelsLayout(..))
import Editor.PanelNode (mkPanelMesh)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic(..), mergeDynArray, subscribeDyn, withLast)
import Model.Roof.ArrayConfig (ArrayConfig(..))
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate(..))
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)

newtype PanelLayerConfig = PanelLayerConfig {

}

derive instance newtypePanelLayerConfig :: Newtype PanelLayerConfig _

newtype PanelLayer = PanelLayer {
    object      :: Object3D,
    disposable  :: Effect Unit,

    roofplate   :: RoofPlate,
    arrayConfig :: Dynamic ArrayConfig,

    layout      :: Dynamic PanelsLayout
}

derive instance newtypePanelLayer :: Newtype PanelLayer _

createPanelLayer :: Array Panel -> ArrayBuilder PanelLayer
createPanelLayer ps = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer


    pure $ PanelLayer {
        object     : layer,
        disposable : d
    }
