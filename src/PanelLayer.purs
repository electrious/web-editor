module Editor.PanelLayer where

import Prelude hiding (add)

import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_)
import Editor.ArrayBuilder (ArrayBuilder)
import Editor.PanelNode (mkPanelMesh)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (subscribe, withLast)
import FRP.Event.Extra (mergeArray)
import Model.Roof.Panel (Panel)
import Three.Core.Object3D (Object3D, add, mkObject3D, remove, setName)

newtype PanelLayer a = PanelLayer {
    wrapper    :: Object3D a,
    disposable :: Effect Unit
}

derive instance newtypePanelLayer :: Newtype (PanelLayer a) _

createPanelLayer :: forall a. Array Panel -> ArrayBuilder (PanelLayer a)
createPanelLayer ps = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer

    -- render all panels
    panelNodeEvts <- mergeArray <$> traverse mkPanelMesh ps
    
    let doRender { last, now } = do
            traverse_ (flip remove layer) $ fromMaybe [] last
            traverse_ (flip add layer) now

    d <- liftEffect $ subscribe (withLast panelNodeEvts) doRender

    pure $ PanelLayer {
        wrapper    : layer,
        disposable : d
    }
