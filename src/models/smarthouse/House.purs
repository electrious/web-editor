module Model.SmartHouse.House where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, fromFoldable, modifyAt, singleton)
import Data.Maybe (fromMaybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_floor, _height, _id, _roofs, _slope)
import Effect (Effect)
import FRP.Event (Event)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Math.Angle (Angle)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.Roof (JSRoof, Roof, exportRoof)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.LAV (_edges)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import Smarthouse.Algorithm.Roofs (generateRoofs)
import Smarthouse.Algorithm.Subtree (Subtree, flipSubtree)
import Three.Math.Vector (Vector3)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
    slope  :: Angle,
    trees  :: List Subtree,
    edges  :: List Edge,
    roofs  :: List Roof
    }

derive instance newtypeHouse :: Newtype House _
derive instance genericHouse :: Generic House _
instance eqHouse :: Eq House where
    eq h1 h2 = h1 ^. _id == h2 ^. _id
instance showHouse :: Show House where
    show = genericShow
instance hasUUIDHouse :: HasUUID House where
    idLens = _id

_trees :: forall t a r. Newtype t { trees :: a | r } => Lens' t a
_trees = _Newtype <<< prop (SProxy :: SProxy "trees")

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    Tuple trees edges <- skeletonize $ singleton $ counterClockPoly poly
    roofs <- generateRoofs slope trees edges
    
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        slope  : slope,
        trees  : trees,
        edges  : edges,
        roofs  : roofs
        }


updateHeight :: Meter -> House -> House
updateHeight height h = h # _height .~ height

-- flip a roof to/from gable
flipRoof :: Int -> House -> Effect House
flipRoof idx h = do
    let vs  = fromFoldable $ h ^. _floor <<< _polyVerts
        ts  = h ^. _trees
        -- flip the subtree at idx
        nts = fromMaybe ts $ modifyAt idx (flip flipSubtree vs) ts
    -- generate new roofs
    roofs <- generateRoofs (h ^. _slope) nts (h ^. _edges)
    pure $ h # _roofs .~ roofs
             # _trees .~ nts

exportHouse :: House -> JSHouse
exportHouse h = JSHouse { id: h ^. idLens, floor: floor, height: meterVal $ h ^. _height, roofs: roofs }
    where floor = vec2Point <$> h ^. _floor <<< _polyVerts
          roofs = Arr.fromFoldable $ exportRoof (h ^. _height) <$> h ^. _roofs

-- House data exported to JSON and saved to server
newtype JSHouse = JSHouse {
    id     :: UUID,
    floor  :: Array Point,
    height :: Number,
    roofs  :: Array JSRoof
    }

derive instance genericJSHouse :: Generic JSHouse _
instance showJSHouse :: Show JSHouse where
    show = genericShow
instance encodeJSHouse :: Encode JSHouse where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeJSHouse :: Decode JSHouse where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })


-- all houses generated data to export
newtype JSHouses = JSHouses {
    houses :: Array JSHouse
    }

derive instance genericJSHouses :: Generic JSHouses _
instance showJSHouses :: Show JSHouses where
    show = genericShow
instance encodeJSHouses :: Encode JSHouses where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

-- | Types of operation applied to houses
data HouseOp = HouseOpCreate House
             | HouseOpDelete UUID
             | HouseOpUpdate House

derive instance genericHouseOp :: Generic HouseOp _
derive instance eqHouseOp :: Eq HouseOp

instance showHouseOp :: Show HouseOp where
    show = genericShow


newtype HouseNode = HouseNode {
    id         :: UUID,
    roofTapped :: Event UUID,
    wallTapped :: Event Unit,
    updated    :: Event HouseOp,
    deleted    :: Event HouseOp
    }

derive instance newtypeHouseNode :: Newtype HouseNode _
instance hasUUIDHouseNode :: HasUUID HouseNode where
    idLens = _id

_roofTapped :: forall t a r. Newtype t { roofTapped :: a | r } => Lens' t a
_roofTapped = _Newtype <<< prop (SProxy :: SProxy "roofTapped")

_wallTapped :: forall t a r. Newtype t { wallTapped :: a | r } => Lens' t a
_wallTapped = _Newtype <<< prop (SProxy :: SProxy "wallTapped")

houseTapped :: HouseNode -> Event UUID
houseTapped h = (const i <$> h ^. _roofTapped) <|> (const i <$> h ^. _wallTapped)
    where i = h ^. idLens
