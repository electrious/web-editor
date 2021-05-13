module Model.SmartHouse.HouseTextureInfo where

import Prelude

import Data.Hardware.Size (Size(..))
import Data.Int (toNumber)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_height, _width)
import Editor.SceneEvent as S
import Three.Loader.TextureLoader (Texture)

newtype HouseTextureInfo = HouseTextureInfo {
    texture       :: Texture,
    imageSize     :: S.Size,
    pixelPerMeter :: Number,
    size          :: Size
    }

derive instance newtypeHouseTextureInfo :: Newtype HouseTextureInfo _

mkHouseTextureInfo :: Texture -> S.Size -> Number -> HouseTextureInfo
mkHouseTextureInfo t imgSz ppm = HouseTextureInfo {
    texture : t,
    imageSize : imgSz,
    pixelPerMeter : ppm,
    size : Size { width  : meter $ toNumber (imgSz ^. _width) / ppm,
                  height : meter $ toNumber (imgSz ^. _height) / ppm
                }
    }

_texture :: forall t a r. Newtype t { texture :: a | r } => Lens' t a
_texture = _Newtype <<< prop (SProxy :: SProxy "texture")

_size :: forall t a r. Newtype t { size :: a | r } => Lens' t a
_size = _Newtype <<< prop (SProxy :: SProxy "size")

_imageSize :: forall t a r. Newtype t { imageSize :: a | r } => Lens' t a
_imageSize = _Newtype <<< prop (SProxy :: SProxy "imageSize")
