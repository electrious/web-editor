module Model.Roof.Panel where

import Prelude hiding (degree)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Default (class Default, def)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map, insert, member, update)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_alignment, _height, _orientation, _slope, _width, _x, _y)
import Effect (Effect)
import Math.Angle (Angle, degree)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent, compX, compY, size)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY)
import Type.Proxy (Proxy(..))

data Orientation = Landscape
                 | Portrait

derive instance Generic Orientation _
derive instance Eq Orientation
instance Show Orientation where
    show = genericShow
instance Ord Orientation where
    compare = genericCompare
instance Bounded Orientation where
    top = genericTop
    bottom = genericBottom
instance Enum Orientation where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum Orientation where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson Orientation where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson Orientation where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for Orientation")

flipOrientation :: Orientation -> Orientation
flipOrientation Landscape = Portrait
flipOrientation Portrait  = Landscape

-- | get general orientation based on a foldable of orientation values
generalOrientation :: forall f. Foldable f => f Orientation -> Orientation
generalOrientation = calcR <<< foldl f (Tuple 0 0)
    where f (Tuple lv pv) Landscape = Tuple (lv + 1) pv
          f (Tuple lv pv) Portrait  = Tuple lv (pv + 1)

          calcR (Tuple lv pv) = if lv >= pv then Landscape else Portrait

data Alignment = Grid
               | Brick

derive instance Generic Alignment _
derive instance Eq Alignment
instance Show Alignment where
    show = genericShow
instance Ord Alignment where
    compare = genericCompare
instance Enum Alignment where
    succ = genericSucc
    pred = genericPred
instance Bounded Alignment where
    top = genericTop
    bottom = genericBottom
instance BoundedEnum Alignment where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson Alignment where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson Alignment where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for Alignment")

newtype Panel = Panel {
    uuid           :: UUID,
    roofplate_uuid :: UUID,
    roofplate_id   :: Int,  -- roofplate_id is required in update panel API
    row_number     :: Int,
    array_number   :: Int,
    x              :: Meter,
    y              :: Meter,
    slope          :: Angle,
    orientation    :: Orientation,
    alignment      :: Alignment
}

derive instance Newtype Panel _
derive instance Generic Panel _
instance Show Panel where
    show = genericShow
instance Eq Panel where
    eq p1 p2 = p1 ^. _uuid == p2 ^. _uuid
instance Ord Panel where
    compare p1 p2 = compare (p1 ^. _uuid) (p2 ^. _uuid)
instance RoofComponent Panel where
    compId = view _uuid
    compX  = view _x
    compY  = view _y
    compZ  = const $ meter 0.0
    size p = case p ^. _orientation of
        Landscape -> def # _width  .~ panelLong
                         # _height .~ panelShort
        Portrait  -> def # _width  .~ panelShort
                         # _height .~ panelLong
instance ArrayComponent Panel where
    arrayNumber p = p ^. _arrNumber
instance EncodeJson Panel where
    encodeJson (Panel p) = "uuid" := p.uuid
                        ~> "roofplate_uuid" := p.roofplate_uuid
                        ~> "roofplate_id" := p.roofplate_id
                        ~> "row_number" := p.row_number
                        ~> "array_number" := p.array_number
                        ~> "x" := p.x
                        ~> "y" := p.y
                        ~> "slope" := p.slope
                        ~> "orientation" := p.orientation
                        ~> "alignment" := p.alignment
                        ~> jsonEmptyObject
instance DecodeJson Panel where
    decodeJson = decodeJson >=> f
        where f o = mkPanel <$> o .: "uuid"
                            <*> o .: "roofplate_uuid"
                            <*> o .: "roofplate_id"
                            <*> o .: "row_number"
                            <*> o .: "array_number"
                            <*> o .: "x"
                            <*> o .: "y"
                            <*> o .: "slope"
                            <*> o .: "orientation"
                            <*> o .: "alignment"

instance Default Panel where
    def = Panel {
        uuid           : emptyUUID,
        roofplate_uuid : emptyUUID,
        roofplate_id   : 0,
        row_number     : 0,
        array_number   : 0,
        x              : meter 0.0,
        y              : meter 0.0,
        slope          : degree 0.0,
        orientation    : Landscape,
        alignment      : Grid
    }

mkPanel :: UUID -> UUID -> Int -> Int -> Int -> Meter -> Meter -> Angle -> Orientation -> Alignment -> Panel
mkPanel u ru ri row an x y slope orientation alignment = Panel { uuid: u, roofplate_uuid: ru, roofplate_id: ri, row_number: row, array_number: an, x: x, y: y, slope: slope, orientation: orientation, alignment: alignment }

_uuid :: forall t a r. Newtype t { uuid :: a | r } => Lens' t a
_uuid = _Newtype <<< prop (Proxy :: Proxy "uuid")

_roofUUID :: forall t a r. Newtype t { roofplate_uuid :: a | r } => Lens' t a
_roofUUID = _Newtype <<< prop (Proxy :: Proxy "roofplate_uuid")

_roofplateId :: forall t a r. Newtype t { roofplate_id :: a | r } => Lens' t a
_roofplateId = _Newtype <<< prop (Proxy :: Proxy "roofplate_id")

_row_number :: forall t a r. Newtype t { row_number :: a | r } => Lens' t a
_row_number = _Newtype <<< prop (Proxy :: Proxy "row_number")

_arrNumber :: forall t a r. Newtype t { array_number :: a | r } => Lens' t a
_arrNumber = _Newtype <<< prop (Proxy :: Proxy "array_number")


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

-- | check if a panel has any changed fields
isDifferent :: Panel -> Panel -> Boolean
isDifferent p1 p2 = p1 ^. _x /= p2 ^. _x ||
                    p1 ^. _y /= p2 ^. _y ||
                    p1 ^. _arrNumber /= p2 ^. _arrNumber ||
                    p1 ^. _row_number /= p2 ^. _row_number ||
                    p1 ^. _orientation /= p2 ^. _orientation ||
                    p1 ^. _alignment /= p2 ^. _alignment


type PanelsDict = Map UUID (List Panel)

panelsDict :: forall f. Foldable f => f Panel -> PanelsDict
panelsDict = foldl f Map.empty
    where f d p = if member (p ^. _roofUUID) d
                  then update (Just <<< (:) p) (p ^. _roofUUID) d
                  else insert (p ^. _roofUUID) (List.singleton p) d

type PanelDict = Map UUID Panel

panelDict :: forall f. Foldable f => f Panel -> PanelDict
panelDict = foldl f Map.empty
    where f d p = insert (p ^. _uuid) p d

-- | get panel vertices list
panelVertices :: Panel -> List Vector3
panelVertices p = v1 : v2 : v3 : v4 : Nil
    where s  = size p
          x  = meterVal $ compX p
          y  = meterVal $ compY p
          w2 = meterVal (s ^. _width) / 2.0
          h2 = meterVal (s ^. _height) / 2.0
          v1 = mkVec3 (x - w2) (y - h2) 0.0
          v2 = mkVec3 (x - w2) (y + h2) 0.0
          v3 = mkVec3 (x + w2) (y + h2) 0.0
          v4 = mkVec3 (x + w2) (y - h2) 0.0

clonePanelTo :: Panel -> Meter -> Meter -> Effect Panel
clonePanelTo p x y = do
    i <- genUUID
    pure $ p # _uuid .~ i
             # _x    .~ x
             # _y    .~ y
