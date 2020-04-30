module Editor.PanelNode where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Three.Core.Object3D (Object3D)

newtype PanelNode a = PanelNode {
    panelId     :: Int,
    panelObject :: Object3D a
}

derive instance newtypePanelNode :: Newtype (PanelNode a) _

_panelId :: forall a. Lens' (PanelNode a) Int
_panelId = _Newtype <<< prop (SProxy :: SProxy "panelId")

_panelObject :: forall a. Lens' (PanelNode a) (Object3D a)
_panelObject = _Newtype <<< prop (SProxy :: SProxy "panelObject")

