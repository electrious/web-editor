module Model.SmartHouse.HouseTextureInfo where

import Prelude

import Data.Hardware.Size (Size(..))
import Data.Int (toNumber)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (meter)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_height, _width)
import Taihe.SceneEvent as S
import FRP.Event (Event)
import Three.Loader.TextureLoader (Texture)
import Type.Proxy (Proxy(..))
import Web.File.File (File)

newtype HouseTextureInfo = HouseTextureInfo {
    texture       :: Texture,
    imageSize     :: S.Size,
    imageFile     :: Event File,
    pixelPerMeter :: Number,
    size          :: Size
    }

derive instance Newtype HouseTextureInfo _

mkHouseTextureInfo :: Texture -> S.Size -> Event File -> Number -> HouseTextureInfo
mkHouseTextureInfo t imgSz img ppm = HouseTextureInfo {
    texture       : t,
    imageSize     : imgSz,
    imageFile     : img,
    pixelPerMeter : ppm,
    size          : Size { width  : meter $ toNumber (imgSz ^. _width) / ppm,
                           height : meter $ toNumber (imgSz ^. _height) / ppm
                         }
    }

_texture :: forall t a r. Newtype t { texture :: a | r } => Lens' t a
_texture = _Newtype <<< prop (Proxy :: Proxy "texture")

_size :: forall t a r. Newtype t { size :: a | r } => Lens' t a
_size = _Newtype <<< prop (Proxy :: Proxy "size")

_imageSize :: forall t a r. Newtype t { imageSize :: a | r } => Lens' t a
_imageSize = _Newtype <<< prop (Proxy :: Proxy "imageSize")

_imageFile :: forall t a r. Newtype t { imageFile :: a | r } => Lens' t a
_imageFile = _Newtype <<< prop (Proxy :: Proxy "imageFile")
