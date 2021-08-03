module Model.SmartHouse.House where

import Prelude hiding (degree)

import Control.Alternative (empty)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, findIndex, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_edge, _edges, _floor, _height, _id, _position, _roofs, _shade, _slope, _vertices)
import Effect (Effect)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Math.Angle (Angle, degree)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly, modifyVertAt)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.Roof (JSRoof, Roof, RoofState(..), exportRoof, roofState)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.HouseParam (houseParamFrom)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.Algorithm.VertInfo (VertWithSlope, updateSlope, vertWithSlope)
import SmartHouse.Algorithm.VertNode (VertNode)
import SmartHouse.ShadeOption (ShadeOption)
import Smarthouse.Algorithm.RoofGeneration (generateRoofs)
import Smarthouse.Algorithm.Subtree (Subtree, flipSubtree)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

newtype House = House {
    id        :: UUID,
    floor     :: Polygon VertWithSlope,
    height    :: Meter,
    slope     :: Angle,
    trees     :: UUIDMap Subtree,
    vertices  :: UUIDMap VertNode,
    edges     :: List Edge,
    roofs     :: UUIDMap Roof,
    peakPoint :: VertNode
    }

derive instance Newtype House _
derive instance Generic House _
instance Default House where
    def = House {
        id        : emptyUUID,
        floor     : def,
        height    : meter 0.0,
        slope     : degree 20.0,
        trees     : M.empty,
        vertices  : M.empty,
        edges     : empty,
        roofs     : M.empty,
        peakPoint : def
        }
instance Eq House where
    eq h1 h2 = h1 ^. _id == h2 ^. _id
instance Show House where
    show = genericShow
instance HasUUID House where
    idLens = _id

_trees :: forall t a r. Newtype t { trees :: a | r } => Lens' t a
_trees = _Newtype <<< prop (Proxy :: Proxy "trees")

_peakPoint :: forall t a r. Newtype t { peakPoint :: a | r } => Lens' t a
_peakPoint = _Newtype <<< prop (Proxy :: Proxy "peakPoint")

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    let floor = flip vertWithSlope slope <$> counterClockPoly poly
    hi <- houseParamFrom floor
    Tuple trees edges <- skeletonize hi
    Tuple roofs nodes <- generateRoofs (S.fromFoldable trees) edges

    let n = findPeakPoint nodes
    
    pure $ House {
        id        : i,
        floor     : floor,
        height    : meter 3.5,   -- default height
        slope     : slope,
        trees     : UM.fromFoldable trees,
        vertices  : nodes,
        edges     : edges,
        roofs     : UM.fromFoldable roofs,
        peakPoint : n
        }


-- find the peak point of all subtrees in a house
findPeakPoint :: forall f. Foldable f => f VertNode -> VertNode
findPeakPoint = foldl (\o n -> if n ^. _height > o ^. _height then n else o) def

updateHeight :: Meter -> House -> House
updateHeight height h = h # _height .~ height


updateSlopes :: Angle -> House -> Effect House
updateSlopes slope h = do
    nh <- updateHouseWith (updateSlope slope <$> h ^. _floor) h
    pure $ nh # _slope .~ slope

updateSlopeForRoof :: Roof -> Angle -> House -> Effect House
updateSlopeForRoof roof slope h = updateHouseWith floor h
    where e     = roof ^. _edge
          idx   = fromMaybe 0 $ findIndex ((==) e) $ h ^. _edges
          floor = modifyVertAt idx (updateSlope slope) (h ^. _floor)


-- rerun the skeletonization algorithm and roof generation on the house with
-- new floor polygon
updateHouseWith :: Polygon VertWithSlope -> House -> Effect House
updateHouseWith floor h = do
    hi <- houseParamFrom floor
    Tuple trees edges <- skeletonize hi
    Tuple roofs nodes <- generateRoofs (S.fromFoldable trees) edges
    pure $ h # _floor    .~ floor
             # _trees    .~ UM.fromFoldable trees
             # _edges    .~ edges
             # _roofs    .~ UM.fromFoldable roofs
             # _vertices .~ nodes
             # _peakPoint .~ findPeakPoint nodes

-- load up to date VertNode
getVertNode :: VertNode -> House -> VertNode
getVertNode v h = fromMaybe v $ M.lookup (v ^. idLens) (h ^. _vertices)


-- flip a roof to/from gable
flipRoof :: UUID -> House -> Effect House
flipRoof i h = do
    let vs  = fromFoldable $ view _position <$> h ^. _floor <<< _polyVerts
        ts  = h ^. _trees
        -- flip the subtree at idx
        nts = M.update (Just <<< flip flipSubtree vs) i ts
    -- generate new roofs
    Tuple roofs nodes <- generateRoofs (S.fromFoldable nts) (h ^. _edges)

    pure $ h # _roofs .~ UM.fromFoldable roofs
             # _trees .~ nts
             # _vertices .~ nodes


updateActiveRoofShade :: ShadeOption -> Maybe UUID -> House -> House
updateActiveRoofShade _ Nothing   h = h
updateActiveRoofShade s (Just ai) h = h # _roofs %~ M.update f ai
    where f r = Just $ set _shade s r

updateActiveRoofSlope :: Angle -> Maybe UUID -> House -> Effect House
updateActiveRoofSlope _ Nothing   h = pure h
updateActiveRoofSlope a (Just ai) h = do
    let roof = M.lookup ai (h ^. _roofs)
        f r = updateSlopeForRoof r a h
    nh <- traverse f roof
    pure $ fromMaybe h nh

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
instance Encode JSHouse where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance Decode JSHouse where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })


-- all houses generated data to export
newtype JSHouses = JSHouses {
    houses :: Array JSHouse
    }

derive instance Generic JSHouses _
instance Show JSHouses where
    show = genericShow
instance Encode JSHouses where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
