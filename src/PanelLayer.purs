module Editor.PanelLayer where

import Prelude hiding (add)

import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_)
import Editor.ArrayBuilder (ArrayBuilder)
import Editor.PanelNode (mkPanelMesh)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (mergeDynArray, subscribeDyn, withLast)
import Model.Roof.Panel (Panel)
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)

newtype PanelLayer = PanelLayer {
    wrapper    :: Object3D,
    disposable :: Effect Unit
}

derive instance newtypePanelLayer :: Newtype PanelLayer _

createPanelLayer :: Array Panel -> ArrayBuilder PanelLayer
createPanelLayer ps = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer

    -- render all panels
    panelNodeDyns <- mergeDynArray <$> traverse mkPanelMesh ps
    
    let doRender { last, now } = do
            traverse_ (flip remove layer) $ fromMaybe [] last
            traverse_ (flip add layer) now

    d <- liftEffect $ subscribeDyn (withLast panelNodeDyns) doRender

    pure $ PanelLayer {
        wrapper    : layer,
        disposable : d
    }
