module Model.Roof.Panel where

import Prelude hiding (degree)

import Algorithm.Segment (Segment, mkSegment)
import Control.Monad.Error.Class (throwError)
import Data.Default (class Default, def)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Hardware.Size (Size(..))
import Data.Lens (Lens', view, (^.), (%~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, emptyUUID, toString)
import Editor.Common.Lenses (_alignment, _arrayNumber, _height, _id, _orientation, _rowNumber, _slope, _width, _x, _y)
import Editor.Common.ProtoCodable (class ProtoEncodable, toProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, defaultOptions, encode, genericDecode, genericEncode)
import Math.Angle (Angle, degree, degreeVal)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class IsPBArrayComp, setArrayNumber, setX, setY)
import Model.Roof.ArrayConfig (ArrayConfig, _gapX)
import Model.RoofComponent (class RoofComponent, size)
import Model.UUID (PBUUID, mkPBUUID, setUUIDString)
import Three.Math.Vector (Vector3, vecX, vecY)
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

flipOrientation :: Orientation -> Orientation
flipOrientation Landscape = Portrait
flipOrientation Portrait  = Landscape

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
    uuid           :: UUID,
    lead_id        :: Int,
    roofplate_id   :: Int,
    roofplate_uuid :: UUID,
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
instance eqPanel :: Eq Panel where
    eq p1 p2 = p1 ^. _uuid == p2 ^. _uuid
instance ordPanel :: Ord Panel where
    compare p1 p2 = compare (p1 ^. _uuid) (p2 ^. _uuid)
instance encodePanel :: Encode Panel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodePanel :: Decode Panel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance roofComponentPanel :: RoofComponent Panel where
    compId = view _uuid
    compX  = view _x
    compY  = view _y
    compZ  = const $ meter 0.0
    size p = case p ^. _orientation of
        Landscape -> def # _width  .~ panelLong
                         # _height .~ panelShort
        Portrait  -> def # _width  .~ panelShort
                         # _height .~ panelLong
instance arrayComponentPanel :: ArrayComponent Panel where
    arrayNumber p = p ^. _arrNumber
instance protoEncodablePanel :: ProtoEncodable Panel PanelPB where
    toProto p = do
        pb <- mkPanelPB
        setId (p ^. _id) pb
        u1 <- mkPBUUID
        setUUIDString (toString $ p ^. _uuid) u1
        setUUID u1 pb

        setLeadId (p ^. _leadId) pb

        u2 <- mkPBUUID
        setUUIDString (toString $ p ^. _roofUUID) u2
        setRoofplateUUID u2 pb
        
        setRowNumber (p ^. _row_number) pb
        setArrayNumber (p ^. _arrNumber) pb
        setX (meterVal $ p ^. _x) pb
        setY (meterVal $ p ^. _y) pb
        setSlope (degreeVal $ p ^. _slope) pb
        orientPb <- toProto $ p ^. _orientation
        setOrientation orientPb pb
        alignPb <- toProto $ p ^. _alignment
        setAlignment alignPb pb

        pure pb
instance defaultPanel :: Default Panel where
    def = Panel {
        id             : 0,
        uuid           : emptyUUID,
        lead_id        : 0,
        roofplate_id   : 0,
        roofplate_uuid : emptyUUID,
        row_number     : 0,
        array_number   : 0,
        x              : meter 0.0,
        y              : meter 0.0,
        slope          : degree 0.0,
        orientation    : Landscape,
        alignment      : Grid
    }
_uuid :: Lens' Panel UUID
_uuid = _Newtype <<< prop (SProxy :: SProxy "uuid")

_leadId :: Lens' Panel Int
_leadId = _Newtype <<< prop (SProxy :: SProxy "lead_id")

_roofId :: Lens' Panel Int
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofplate_id")

_roofUUID :: Lens' Panel UUID
_roofUUID = _Newtype <<< prop (SProxy :: SProxy "roofplate_uuid")

_row_number :: Lens' Panel Int
_row_number = _Newtype <<< prop (SProxy :: SProxy "row_number")

_arrNumber :: Lens' Panel Int
_arrNumber = _Newtype <<< prop (SProxy :: SProxy "array_number")


-- | panel default width and height
panelLong :: Meter
panelLong = meter 1.6

panelShort :: Meter
panelShort = meter 1.0

-- | Get validated slope value for any value that has slope.
-- only slope angles larger than 5 degrees are considered valid slope
validatedSlope :: forall a r. Newtype a { slope :: Angle | r } => a -> Maybe Angle
validatedSlope p | p ^. _slope < degree 5.0 = Nothing
                 | otherwise                = Just $ p ^. _slope


addDelta :: Vector3 -> Panel -> Panel
addDelta delta p = p # _x %~ (+) (meter $ vecX delta)
                     # _y %~ (+) (meter $ vecY delta)

panelSegment :: ArrayConfig -> Panel -> Segment (List Panel)
panelSegment cfg p = mkSegment (x - w2 - gapX2) (x + w2 + gapX2) y (List.singleton p)
    where s = size p
          w2 = meterVal (s ^. _width) / 2.0
          gapX2 = meterVal (cfg ^. _gapX) / 2.0
          x = meterVal $ p ^. _x
          y = meterVal $ p ^. _y

-- | check if a panel has any changed fields
isDifferent :: Panel -> Panel -> Boolean
isDifferent p1 p2 = p1 ^. _x /= p2 ^. _x ||
                    p1 ^. _y /= p2 ^. _y ||
                    p1 ^. _arrNumber /= p2 ^. _arrNumber ||
                    p1 ^. _row_number /= p2 ^. _row_number ||
                    p1 ^. _orientation /= p2 ^. _orientation ||
                    p1 ^. _alignment /= p2 ^. _alignment
