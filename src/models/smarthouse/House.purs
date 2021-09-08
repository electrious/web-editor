module Model.SmartHouse.House where

import Prelude hiding (degree)

import Control.Alternative (empty)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, concatMap, findIndex)
import Data.Map (values)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Data.UUIDWrapper (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_edges, _floor, _height, _id, _position, _roofs)
import Effect (Effect)
import Math.Angle (Angle, degree)
import Math.LineSeg (LineSeg)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly, modifyVertAt)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.Chimney (Chimney, ChimneyOp(..))
import Model.SmartHouse.Roof (JSRoof, Roof, RoofState(..), exportRoof, roofState)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, _lineEdge)
import SmartHouse.Algorithm.HouseParam (houseParamFrom)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.Algorithm.VertInfo (VertWithSlope, updateSlope, vertWithSlope)
import SmartHouse.SlopeOption (SlopeOption(..))
import Smarthouse.Algorithm.RoofGeneration (generateRoofs)
import Smarthouse.Algorithm.Subtree (Subtree, treeLines)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))


defaultSlope :: Angle
defaultSlope = degree 30.0

newtype House = House {
    id        :: UUID,
    floor     :: Polygon VertWithSlope,
    height    :: Meter,
    trees     :: UUIDMap Subtree,
    edges     :: List Edge,
    roofs     :: UUIDMap Roof,
    chimneys  :: UUIDMap Chimney
    }

derive instance Newtype House _
derive instance Generic House _
instance Default House where
    def = House {
        id        : emptyUUID,
        floor     : def,
        height    : meter 0.0,
        trees     : M.empty,
        edges     : empty,
        roofs     : M.empty,
        chimneys  : M.empty
        }
instance Eq House where
    eq h1 h2 = h1 ^. _id == h2 ^. _id
instance Show House where
    show = genericShow
instance HasUUID House where
    idLens = _id

_trees :: forall t a r. Newtype t { trees :: a | r } => Lens' t a
_trees = _Newtype <<< prop (Proxy :: Proxy "trees")

_chimneys :: forall t a r. Newtype t { chimneys :: a | r } => Lens' t a
_chimneys = _Newtype <<< prop (Proxy :: Proxy "chimneys")

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    floor <- traverse (flip vertWithSlope slope) (counterClockPoly poly)
    let hi = houseParamFrom floor
    Tuple trees edges <- skeletonize hi
    let roofs = generateRoofs (S.fromFoldable trees) edges

    pure $ House {
        id        : i,
        floor     : floor,
        height    : meter 3.5,   -- default height
        trees     : UM.fromFoldable trees,
        edges     : edges,
        roofs     : UM.fromFoldable roofs,
        chimneys  : M.empty
        }

updateHeight :: Meter -> House -> House
updateHeight height h = h # _height .~ height

updateHouseSlope :: SlopeOption -> Maybe UUID -> House -> Effect House
updateHouseSlope (ActiveRoof a) id h = updateActiveRoofSlope a id h
updateHouseSlope (AllRoofs a)   _  h = updateHouseWith (updateSlope a <$> h ^. _floor) h

updateHouseChimney :: ChimneyOp -> House -> House
updateHouseChimney (ChimCreate c) h = h # _chimneys %~ M.insert (c ^. idLens) c
updateHouseChimney (ChimDelete i) h = h # _chimneys %~ M.delete i
updateHouseChimney (ChimUpdate c) h = h # _chimneys %~ M.update (const $ Just c) (c ^. idLens)

updateActiveRoofSlope :: Angle -> Maybe UUID -> House -> Effect House
updateActiveRoofSlope _ Nothing   h = pure h
updateActiveRoofSlope a (Just ai) h = do
    let roof = M.lookup ai (h ^. _roofs)
        f r = updateSlopeForRoof r a h
    nh <- traverse f roof
    pure $ fromMaybe h nh

updateSlopeForRoof :: Roof -> Angle -> House -> Effect House
updateSlopeForRoof roof slope h = updateHouseWith floor h
    where es    = roof ^. _edges
          getIdx e = fromMaybe 0 $ findIndex ((==) e) $ h ^. _edges
          idxs = getIdx <$> es
          floor = foldl (\f idx -> modifyVertAt idx (updateSlope slope) f) (h ^. _floor) idxs


-- rerun the skeletonization algorithm and roof generation on the house with
-- new floor polygon
updateHouseWith :: Polygon VertWithSlope -> House -> Effect House
updateHouseWith floor h = do
    let hi = houseParamFrom floor
    Tuple trees edges <- skeletonize hi
    let roofs = generateRoofs (S.fromFoldable trees) edges

    pure $ h # _floor    .~ floor
             # _trees    .~ UM.fromFoldable trees
             # _edges    .~ edges
             # _roofs    .~ UM.fromFoldable roofs

getHouseLines :: House -> List (LineSeg Vector3)
getHouseLines h = tLines <> eLines
    where tls = concatMap treeLines (values $ h ^. _trees)
          tLines = map (view _position) <$> tls
          eLines = view _lineEdge <$> h ^. _edges

exportHouse :: House -> JSHouse
exportHouse h = JSHouse { id: h ^. idLens, floor: floor, height: meterVal $ h ^. _height, roofs: roofs }
    where floor = vec2Point <<< view _position <$> h ^. _floor <<< _polyVerts
          roofs = Arr.fromFoldable $ exportRoof (h ^. _height) <$> filter ((==) SlopeRoof <<< roofState) (h ^. _roofs)

-- House data exported to JSON and saved to server
newtype JSHouse = JSHouse {
    id     :: UUID,
    floor  :: Array Point,
    height :: Number,
    roofs  :: Array JSRoof
    }

derive instance Generic JSHouse _
instance Show JSHouse where
    show = genericShow
instance EncodeJson JSHouse where
    encodeJson (JSHouse h) = "id" := h.id
                          ~> "floor" := h.floor
                          ~> "height" := h.height
                          ~> "roofs" := h.roofs
                          ~> jsonEmptyObject
instance DecodeJson JSHouse where
    decodeJson = decodeJson >=> f
        where f o = mkJSHouse <$> o .: "id"
                              <*> o .: "floor"
                              <*> o .: "height"
                              <*> o .: "roofs"

mkJSHouse :: UUID -> Array Point -> Number -> Array JSRoof -> JSHouse
mkJSHouse id floor height roofs = JSHouse { id, floor, height, roofs }

-- all houses generated data to export
newtype JSHouses = JSHouses {
    houses :: Array JSHouse
    }

derive instance Generic JSHouses _
instance Show JSHouses where
    show = genericShow
instance EncodeJson JSHouses where
    encodeJson (JSHouses h) = "houses" := h.houses
                           ~> jsonEmptyObject
