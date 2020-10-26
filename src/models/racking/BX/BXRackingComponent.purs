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

foreign import data BallastComponentPB :: Type
foreign import mkBallastComponentPB :: Effect BallastComponentPB

instance hasArrayNumberBallastComponentPB :: HasArrayNumber BallastComponentPB

foreign import getChassis :: BallastComponentPB -> Array ChassisPB
foreign import setChassis :: Array ChassisPB -> BallastComponentPB -> Effect Unit
foreign import getBlocks :: BallastComponentPB -> Array BlockPB
foreign import setBlocks :: Array BlockPB -> BallastComponentPB -> Effect Unit


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
