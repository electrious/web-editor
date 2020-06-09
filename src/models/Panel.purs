module Model.Roof.Panel where

import Prelude hiding (degree)

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Hardware.Size (Size(..))
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_alignment, _id, _orientation, _slope, _x, _y)
import Editor.Common.ProtoCodable (class ProtoEncodable, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, defaultOptions, encode, genericDecode, genericEncode)
import Math.Angle (Angle, degree, degreeVal)
import Model.Class (class IsPBArrayComp, setArrayNumber, setX, setY)
import Model.UUID (PBUUID, mkPBUUID, setUUIDString)
import Util (ffi, fpi)

newtype OrientationPB = OrientationPB Int
derive newtype instance eqOrientationPB :: Eq OrientationPB
foreign import orientationInvalid   :: OrientationPB
foreign import orientationLandscape :: OrientationPB
foreign import orientationPortrait  :: OrientationPB

newtype AlignmentPB = AlignmentPB Int
derive newtype instance eqAlignmentPB :: Eq AlignmentPB
foreign import alignmentInvalid :: AlignmentPB
foreign import alignmentGrid    :: AlignmentPB
foreign import alignmentBrick   :: AlignmentPB

foreign import data PanelPB :: Type
foreign import mkPanelPB :: Effect PanelPB

instance isPBArrayCompPanelPB :: IsPBArrayComp PanelPB

getId :: PanelPB -> Int
getId = ffi ["p"] "p.getId()"

setId :: Int -> PanelPB -> Effect Unit
setId = fpi ["i", "p", ""] "p.setId(i)"

getUUID :: PanelPB -> PBUUID
getUUID = ffi ["p"] "p.getUuid()"

setUUID :: PBUUID -> PanelPB -> Effect Unit
setUUID = fpi ["u", "p", ""] "p.setUuid(u)"

getLeadId :: PanelPB -> Int
getLeadId = ffi ["p"] "p.getLeadId()"

setLeadId :: Int -> PanelPB -> Effect Unit
setLeadId = fpi ["i", "p", ""] "p.setLeadId(i)"

getRoofplateUUID :: PanelPB -> PBUUID
getRoofplateUUID = ffi ["p"] "p.getRoofplateUuid()"

setRoofplateUUID :: PBUUID -> PanelPB -> Effect Unit
setRoofplateUUID = fpi ["u", "p", ""] "p.setRoofplateUuid(u)"

getRowNumber :: PanelPB -> Int
getRowNumber = ffi ["p"] "p.getRowNumber()"

setRowNumber :: Int -> PanelPB -> Effect Unit
setRowNumber = fpi ["r", "p", ""] "p.setRowNumber(r)"

getSlope :: PanelPB -> Number
getSlope = ffi ["p"] "p.getSlope()"

setSlope :: Number -> PanelPB -> Effect Unit
setSlope = fpi ["s", "p", ""] "p.setSlope(s)"

getOrientation :: PanelPB -> OrientationPB
getOrientation = ffi ["p"] "p.getOrientation()"

setOrientation :: OrientationPB -> PanelPB -> Effect Unit
setOrientation = fpi ["o", "p", ""] "p.setOrientation(o)"

getAlignment :: PanelPB -> AlignmentPB
getAlignment = ffi ["p"] "p.getAlignment()"

setAlignment :: AlignmentPB -> PanelPB -> Effect Unit
setAlignment = fpi ["a", "p", ""] "p.setAlignment(a)"


data Orientation = Landscape
                 | Portrait

derive instance genericOrient :: Generic Orientation _
derive instance eqOrient :: Eq Orientation
instance showOrient :: Show Orientation where
    show = genericShow
instance ordOrient :: Ord Orientation where
    compare = genericCompare
instance boundOrient :: Bounded Orientation where
    top = genericTop
    bottom = genericBottom
instance enumOrient :: Enum Orientation where
    succ = genericSucc
    pred = genericPred
instance boundEnumOrient :: BoundedEnum Orientation where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeOrient :: Encode Orientation where
    encode = fromEnum >>> encode
instance decodeOrient :: Decode Orientation where
    decode o = decode o >>= \i -> do 
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError ("can't decode Orientation from " <> show i)

instance protoEncodableOrientation :: ProtoEncodable Orientation OrientationPB where
    toProto Landscape = pure orientationLandscape
    toProto Portrait  = pure orientationPortrait

data Alignment = Grid
               | Brick

derive instance genericAlign :: Generic Alignment _
derive instance eqAlign :: Eq Alignment
instance showAlign :: Show Alignment where
    show = genericShow
instance ordAlign :: Ord Alignment where
    compare = genericCompare
instance enumAlign :: Enum Alignment where
    succ = genericSucc
    pred = genericPred
instance bounedAlign :: Bounded Alignment where
    top = genericTop
    bottom = genericBottom
instance boundedEnumAlign :: BoundedEnum Alignment where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeAlign :: Encode Alignment where
    encode = fromEnum >>> encode
instance decodeAlign :: Decode Alignment where
    decode a = decode a >>= \i -> do
                   case toEnum i of
                      Just v -> pure v
                      Nothing -> throwError $ singleton $ ForeignError ("can't decode Alignment from " <> show i)

instance protoEncodableAlignment :: ProtoEncodable Alignment AlignmentPB where
    toProto Grid  = pure alignmentGrid
    toProto Brick = pure alignmentBrick

newtype Panel = Panel {
    id             :: Int,
    uuid           :: String,
    lead_id        :: Int,
    roofplate_id   :: Int,
    roofplate_uuid :: String,
    row_number     :: Int,
    array_number   :: Int,
    x              :: Meter,
    y              :: Meter,
    slope          :: Angle,
    orientation    :: Orientation,
    alignment      :: Alignment
}

derive instance newtypePanel :: Newtype Panel _
derive instance genericPanel :: Generic Panel _
instance showPanel :: Show Panel where
    show = genericShow
instance encodePanel :: Encode Panel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodePanel :: Decode Panel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance protoEncodablePanel :: ProtoEncodable Panel PanelPB where
    toProto p = do
        pb <- mkPanelPB
        setId (p ^. _id) pb
        u1 <- mkPBUUID
        setUUIDString (p ^. _uuid) u1
        setUUID u1 pb

        setLeadId (p ^. _leadId) pb

        u2 <- mkPBUUID
        setUUIDString (p ^. _roofUUID) u2
        setRoofplateUUID u2 pb
        
        setRowNumber (p ^. _rowNumber) pb
        setArrayNumber (p ^. _arrNumber) pb
        setX (meterVal $ p ^. _x) pb
        setY (meterVal $ p ^. _y) pb
        setSlope (degreeVal $ p ^. _slope) pb
        orientPb <- toProto $ p ^. _orientation
        setOrientation orientPb pb
        alignPb <- toProto $ p ^. _alignment
        setAlignment alignPb pb

        pure pb

_uuid :: Lens' Panel String
_uuid = _Newtype <<< prop (SProxy :: SProxy "uuid")

_leadId :: Lens' Panel Int
_leadId = _Newtype <<< prop (SProxy :: SProxy "lead_id")

_roofId :: Lens' Panel Int
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofplate_id")

_roofUUID :: Lens' Panel String
_roofUUID = _Newtype <<< prop (SProxy :: SProxy "roofplate_uuid")

_rowNumber :: Lens' Panel Int
_rowNumber = _Newtype <<< prop (SProxy :: SProxy "row_number")

_arrNumber :: Lens' Panel Int
_arrNumber = _Newtype <<< prop (SProxy :: SProxy "array_number")


-- | panel default width and height
panelLong :: Meter
panelLong = meter 1.6

panelShort :: Meter
panelShort = meter 1.0

panelSize :: Panel -> Size
panelSize p = case p ^. _orientation of
    Landscape -> Size { width: panelLong, height: panelShort }
    Portrait  -> Size { width: panelShort, height: panelLong }

-- only slope angles larger than 5 degrees are considered valid slope
validatedSlope :: Panel -> Maybe Angle
validatedSlope p | p ^. _slope < degree 5.0 = Nothing
                 | otherwise                = Just $ p ^. _slope
