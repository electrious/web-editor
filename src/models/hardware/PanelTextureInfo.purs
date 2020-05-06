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
    standard   :: Maybe String,
    premium    :: Maybe String,
    standard72 :: Maybe String
}

derive instance newtypePanelTextureInfo :: Newtype PanelTextureInfo _
derive instance genericPanelTextureInfo :: Generic PanelTextureInfo _
instance defaultPanelTextureInfo :: Default PanelTextureInfo where
    def = PanelTextureInfo { standard   : Nothing,
                             premium    : Nothing,
                             standard72 : Nothing
                           }

_standard :: Lens' PanelTextureInfo (Maybe String)
_standard = _Newtype <<< prop (SProxy :: SProxy "standard")

_premium :: Lens' PanelTextureInfo (Maybe String)
_premium = _Newtype <<< prop (SProxy :: SProxy "premium")

_standard72 :: Lens' PanelTextureInfo (Maybe String)
_standard72 = _Newtype <<< prop (SProxy :: SProxy "standard72")
