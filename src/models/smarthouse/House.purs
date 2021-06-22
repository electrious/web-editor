module Model.SmartHouse.House where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_floor, _height, _id, _roofs, _slope)
import Editor.RoofManager (ArrayEvents)
import Effect (Effect)
import FRP.Event (Event)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Math.Angle (Angle, degree)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.Roof (JSRoof, Roof, exportRoof, updateShadeOption)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.LAV (_edges)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.ShadeOption (ShadeOption)
import Smarthouse.Algorithm.Roofs (generateRoofs)
import Smarthouse.Algorithm.Subtree (Subtree, flipSubtree)
import Three.Math.Vector (Vector3)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
    slope  :: Angle,
    trees  :: UUIDMap Subtree,
    edges  :: List Edge,
    roofs  :: UUIDMap Roof
    }

derive instance newtypeHouse :: Newtype House _
derive instance genericHouse :: Generic House _
instance defaultHouse :: Default House where
    def = House {
        id     : emptyUUID,
        floor  : def,
        height : meter 0.0,
        slope  : degree 20.0,
        trees  : M.empty,
        edges  : empty,
        roofs  : M.empty
        }
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
    Tuple trees edges <- skeletonize $ counterClockPoly poly
    roofs <- generateRoofs slope (S.fromFoldable trees) edges
    
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        slope  : slope,
        trees  : UM.fromFoldable trees,
        edges  : edges,
        roofs  : UM.fromFoldable roofs
        }


updateHeight :: Meter -> House -> House
updateHeight height h = h # _height .~ height

getRoof :: Maybe UUID -> House -> Maybe Roof
getRoof Nothing _  = Nothing
getRoof (Just i) h = M.lookup i $ h ^. _roofs

-- flip a roof to/from gable
flipRoof :: UUID -> House -> Effect House
flipRoof i h = do
    let vs  = fromFoldable $ h ^. _floor <<< _polyVerts
        ts  = h ^. _trees
        -- flip the subtree at idx
        nts = M.update (Just <<< flip flipSubtree vs) i ts
    -- generate new roofs
    roofs <- generateRoofs (h ^. _slope) (S.fromFoldable nts) (h ^. _edges)
    pure $ h # _roofs .~ UM.fromFoldable roofs
             # _trees .~ nts


updateActiveRoofShade :: ShadeOption -> Maybe UUID -> House -> House
updateActiveRoofShade s Nothing   h = h
updateActiveRoofShade s (Just ai) h = h # _roofs %~ M.update f ai
    where f r = Just $ updateShadeOption r s

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
    id          :: UUID,
    roofTapped  :: Event UUID,
    wallTapped  :: Event Unit,
    updated     :: Event HouseOp,
    deleted     :: Event HouseOp,
    activeRoof  :: Event (Maybe Roof),

    arrayEvents :: ArrayEvents
    }

derive instance newtypeHouseNode :: Newtype HouseNode _
instance hasUUIDHouseNode :: HasUUID HouseNode where
    idLens = _id
instance defaultHouseNode :: Default HouseNode where
    def = HouseNode {
        id          : emptyUUID,
        roofTapped  : empty,
        wallTapped  : empty,
        updated     : empty,
        deleted     : empty,
        activeRoof  : empty,

        arrayEvents : def
        }

_roofTapped :: forall t a r. Newtype t { roofTapped :: a | r } => Lens' t a
_roofTapped = _Newtype <<< prop (SProxy :: SProxy "roofTapped")

_wallTapped :: forall t a r. Newtype t { wallTapped :: a | r } => Lens' t a
_wallTapped = _Newtype <<< prop (SProxy :: SProxy "wallTapped")

_activeRoof :: forall t a r. Newtype t { activeRoof :: a | r } => Lens' t a
_activeRoof = _Newtype <<< prop (SProxy :: SProxy "activeRoof")

houseTapped :: HouseNode -> Event UUID
houseTapped h = (const i <$> h ^. _roofTapped) <|> (const i <$> h ^. _wallTapped)
    where i = h ^. idLens
