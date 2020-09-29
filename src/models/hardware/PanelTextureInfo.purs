module Model.Hardware.PanelTextureInfo where

import Prelude

import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

-- texture info needed to render panels
newtype PanelTextureInfo = PanelTextureInfo {
    standard            :: Maybe String,
    premium             :: Maybe String,
    standard72          :: Maybe String,
    rotateButtonTexture :: Maybe String
}

derive instance newtypePanelTextureInfo :: Newtype PanelTextureInfo _
derive instance genericPanelTextureInfo :: Generic PanelTextureInfo _
instance defaultPanelTextureInfo :: Default PanelTextureInfo where
    def = PanelTextureInfo { standard            : Nothing,
                             premium             : Nothing,
                             standard72          : Nothing,
                             rotateButtonTexture : Nothing
                           }

_standard :: forall t a r . Newtype t { standard :: a | r } => Lens' t a
_standard = _Newtype <<< prop (SProxy :: SProxy "standard")

_premium :: forall t a r . Newtype t { premium :: a | r } => Lens' t a
_premium = _Newtype <<< prop (SProxy :: SProxy "premium")

_standard72 :: forall t a r . Newtype t { standard72 :: a | r } => Lens' t a
_standard72 = _Newtype <<< prop (SProxy :: SProxy "standard72")
