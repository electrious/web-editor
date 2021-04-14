module Model.SmartHouse.House where

import Prelude

import Control.Alt ((<|>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, singleton)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_floor, _height, _id, _name, _position, _roofs, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt)
import Math.Angle (Angle)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo)
import Model.SmartHouse.Roof (Roof, createRoofFrom, renderRoof)
import Model.UUID (class HasUUID, idLens)
import Rendering.Node (Node, node, tapMesh)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, toVec2)

newtype House = House {
    id     :: UUID,
    floor  :: Polygon Vector3,
    height :: Meter,
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

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    roofPolys <- skeletonize slope $ singleton $ counterClockPoly poly
    roofs <- traverse createRoofFrom roofPolys
    
    pure $ House {
        id     : i,
        floor  : poly,
        height : meter 3.5,   -- default height
        roofs  : roofs
        }

newtype HouseNode = HouseNode {
    id         :: UUID,
    roofTapped :: Event UUID,
    wallTapped :: Event Unit
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

-- rendering
renderHouse :: Boolean -> House -> Node HouseTextureInfo HouseNode
renderHouse active house = do
    let h = house ^. _height
        p = mkVec3 0.0 0.0 (meterVal h)
    wallTap <- renderWalls h $ house ^. _floor
    roofTap <- node (def # _position .~ pure p) $ traverse renderRoof $ house ^. _roofs
    pure $ HouseNode {
        id         : house ^. idLens,
        roofTapped : anyEvt roofTap,
        wallTapped : wallTap
        }

renderWalls :: forall e. Meter -> Polygon Vector3 -> Node e (Event Unit)
renderWalls height poly = do
    shp <- liftEffect $ mkShape $ (toVec2 <$> poly) ^. _polyVerts
    geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ meterVal height
                                                    # _bevelEnabled .~ false
    mat <- liftEffect $ mkMeshPhongMaterial 0x999999

    m <- tapMesh (def # _name .~ "walls") geo mat
    pure $ const unit <$> m ^. _tapped
