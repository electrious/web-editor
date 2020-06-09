module Model.Racking.BX.BXRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.BX.Block (Block, BlockPB)
import Model.Racking.BX.Chassis (Chassis, ChassisPB)
import Model.Racking.Class (class HasArrayNumber, getArrayNumber)
import Util (ffi, fpi)

foreign import data BallastComponentPB :: Type
foreign import mkBallastComponentPB :: Effect BallastComponentPB

instance hasArrayNumberBallastComponentPB :: HasArrayNumber BallastComponentPB

getChassis :: BallastComponentPB -> Array ChassisPB
getChassis = ffi ["b"] "b.getChassisList()"

setChassis :: Array ChassisPB -> BallastComponentPB -> Effect Unit
setChassis = fpi ["cs", "b", ""] "b.setChassisList(cs)"

getBlocks :: BallastComponentPB -> Array BlockPB
getBlocks = ffi ["b"] "b.getBlocksList()"

setBlocks :: Array BlockPB -> BallastComponentPB -> Effect Unit
setBlocks = fpi ["bs", "b", ""] "b.setBlocksList(bs)"


newtype BXRackingComponent = BXRackingComponent {
    arrayNumber :: Int,
    chassis     :: Array Chassis,
    blocks      :: Array Block
}

derive instance newtypeBXRackingComponent :: Newtype BXRackingComponent _
derive instance genericBXRackingComponent :: Generic BXRackingComponent _
instance showBXRackingComponent :: Show BXRackingComponent where
    show = genericShow
instance protoDecodableBXRackingComponent :: ProtoDecodable BXRackingComponent BallastComponentPB where
    fromProto c = BXRackingComponent {
        arrayNumber : getArrayNumber c,
        chassis     : fromProto <$> getChassis c,
        blocks      : fromProto <$> getBlocks c
    }

newtype BXRackingNumbers = BXRackingNumbers {
    chassis :: Int,
    blocks  :: Int
}

derive instance newtypeBXRackingNumbers :: Newtype BXRackingNumbers _
derive instance genericBXRackingNumbers :: Generic BXRackingNumbers _
instance showBXRackingNumbers :: Show BXRackingNumbers where
    show = genericShow
