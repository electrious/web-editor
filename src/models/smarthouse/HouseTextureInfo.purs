module Model.SmartHouse.HouseTextureInfo where

import Prelude

import Data.Hardware.Size (Size)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Three.Loader.TextureLoader (Texture)

newtype HouseTextureInfo = HouseTextureInfo {
    texture :: Texture,
    size    :: Size
    }

derive instance newtypeHouseTextureInfo :: Newtype HouseTextureInfo _

mkHouseTextureInfo :: Texture -> Size -> HouseTextureInfo
mkHouseTextureInfo t s = HouseTextureInfo { texture : t, size : s }

_texture :: forall t a r. Newtype t { texture :: a | r } => Lens' t a
_texture = _Newtype <<< prop (SProxy :: SProxy "texture")

_size :: forall t a r. Newtype t { size :: a | r } => Lens' t a
_size = _Newtype <<< prop (SProxy :: SProxy "size")
