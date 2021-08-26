module Model.SmartHouse.Chimney where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~), (^.))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.UUIDWrapper (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _length, _position, _width)
import Effect (Effect)
import FRP.Event (Event)
import Model.UUID (class HasUUID, idLens)
import Three.Math.Vector (Vector3, mkVec3)

newtype Chimney = Chimney {
    id       :: UUID,
    position :: Vector3,
    length   :: Meter,
    width    :: Meter,
    height   :: Meter
}

derive instance Newtype Chimney _
derive instance Generic Chimney _
instance HasUUID Chimney where
    idLens = _id
instance Eq Chimney where
    eq c1 c2 = c1 ^. idLens == c2 ^. idLens
instance Default Chimney where
    def = Chimney {
        id       : emptyUUID,
        position : def,
        length   : meter 0.5,
        width    : meter 0.5,
        height   : meter 1.5
    }


mkChimney :: Vector3 -> Effect Chimney
mkChimney pos = do
    i <- genUUID
    pure $ def # _id       .~ i
               # _position .~ pos


chimneyScale :: Chimney -> Vector3
chimneyScale c = mkVec3 (meterVal $ c ^. _length)
                        (meterVal $ c ^. _width)
                        (meterVal $ c ^. _height)

data ChimneyOp = ChimCreate Chimney
               | ChimDelete UUID
               | ChimUpdate Chimney
derive instance Eq ChimneyOp


newtype ChimneyNode = ChimneyNode {
    id      :: UUID,
    tapped  :: Event UUID,
    updated :: Event ChimneyOp
}

derive instance Newtype ChimneyNode _
instance HasUUID ChimneyNode where
    idLens = _id
instance Default ChimneyNode where
    def = ChimneyNode {
        id      : emptyUUID,
        tapped  : empty,
        updated : empty
    }
