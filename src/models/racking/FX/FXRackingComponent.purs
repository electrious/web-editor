module Model.Racking.FX.FXRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Class (class HasArrayNumber, class HasFlashes, getArrayNumber, getFlashes)
import Model.Racking.FX.Bridge (Bridge, BridgePB)
import Model.Racking.FX.EndCap (EndCap, EndCapPB)
import Model.Racking.FX.Mount (Mount, MountPB)
import Model.Racking.FX.Skirt (Skirt, SkirtPB)
import Model.Racking.Flash (Flash)

foreign import data RailFreeComponentPB :: Type
foreign import mkRailFreeComponentPB :: Effect RailFreeComponentPB

instance hasArrayNumberRailFreeComponentPB :: HasArrayNumber RailFreeComponentPB
instance hasFlashesRailFreeComponentPB :: HasFlashes RailFreeComponentPB

foreign import getMounts :: RailFreeComponentPB -> Array MountPB
foreign import setMounts :: Array MountPB -> RailFreeComponentPB -> Effect Unit
foreign import getBridges :: RailFreeComponentPB -> Array BridgePB
foreign import setBridges :: Array BridgePB -> RailFreeComponentPB -> Effect Unit
foreign import getSkirts :: RailFreeComponentPB -> Array SkirtPB
foreign import setSkirts :: Array SkirtPB -> RailFreeComponentPB -> Effect Unit
foreign import getLeftCaps :: RailFreeComponentPB -> Array EndCapPB
foreign import setLeftCaps :: Array EndCapPB -> RailFreeComponentPB -> Effect Unit
foreign import getRightCaps :: RailFreeComponentPB -> Array EndCapPB
foreign import setRightCaps :: Array EndCapPB -> RailFreeComponentPB -> Effect Unit

newtype FXRackingComponent = FXRackingComponent {
    arrayNumber  :: Int,
    flashes      :: Array Flash,
    mounts       :: Array Mount,
    bridges      :: Array Bridge,
    skirts       :: Array Skirt,
    leftEndCaps  :: Array EndCap,
    rightEndCaps :: Array EndCap
}

derive instance newtypeFXRackingComponent :: Newtype FXRackingComponent _
derive instance genericFXRackingComponent :: Generic FXRackingComponent _
instance showFXRackingComponent :: Show FXRackingComponent where
    show = genericShow
instance protoDecodableRXRackingComponent :: ProtoDecodable FXRackingComponent RailFreeComponentPB where
    fromProto c = FXRackingComponent {
        arrayNumber  : getArrayNumber c,
        flashes      : fromProto <$> getFlashes c,
        mounts       : fromProto <$> getMounts c,
        bridges      : fromProto <$> getBridges c,
        skirts       : fromProto <$> getSkirts c,
        leftEndCaps  : fromProto <$> getLeftCaps c,
        rightEndCaps : fromProto <$> getRightCaps c
    }

newtype FXRackingNumbers = FXRackingNumbers {
    flashes      :: Int,
    mounts       :: Int,
    bridges      :: Int,
    skirts       :: Int,
    leftEndCaps  :: Int,
    rightEndCaps :: Int
}

derive instance newtypeFXRackingNumbers :: Newtype FXRackingNumbers _
derive instance genericFXRackingNumbers :: Generic FXRackingNumbers _
instance showFXRackingNumbers :: Show FXRackingNumbers where
    show = genericShow
