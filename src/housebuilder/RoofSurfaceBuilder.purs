module HouseBuilder.RoofSurfaceBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (concat, find)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _face, _floor, _modeDyn, _mouseMove, _name, _point, _polygon, _position)
import Editor.ObjectAdder (CandidatePoint, createObjectAdder, mkCandidatePoint)
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, dynEvent, gateDyn, step)
import FRP.Event (Event, fold, keepLatest)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseBuilder.RoofSurface (RoofSurface, newSurface, surfaceAround)
import Model.Polygon (class PolyVertex, Polygon, _polyVerts, getPos, polyCenter, polyMidPoints)
import Model.UUID (idLens)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeE, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (dist, mkVec3)
import Util (foldEvtWith)


-- | RoofSurface Editor to edit a single RoofSurface

newtype RoofSurfEditor = RoofSurfEditor {
    surface :: Event RoofSurface,
    delete  :: Event UUID
    }

derive instance newtypeRoofSurfEditor :: Newtype RoofSurfEditor _
instance defaultRoofSurfEditor :: Default RoofSurfEditor where
    def = RoofSurfEditor {
        surface : empty,
        delete  : empty
        }

_surface :: forall t a r. Newtype t { surface :: a | r } => Lens' t a
_surface = _Newtype <<< prop (SProxy :: SProxy "surface")

editRoofSurface :: forall e. RoofSurface -> Node e RoofSurfEditor
editRoofSurface rs = do
    let cfg = def # _active  .~ pure Active
                  # _polygon .~ rs ^. _polygon
                  
    editor <- createPolyEditor cfg
    
    pure $ def # _surface .~ performEvent (newSurface <$> editor ^. _polygon)
               # _delete  .~ (const (rs ^. idLens) <$> editor ^. _delete)


-- | get all vertices, mid points and center point to avoid showing surface adder near them
floorPlanPoints :: forall v. PolyVertex v => Polygon v -> Array v
floorPlanPoints poly = concat [[polyCenter poly], poly ^. _polyVerts, snd <$> polyMidPoints poly]

validCandPoint :: forall v. PolyVertex v => CandidatePoint -> Polygon v -> Boolean
validCandPoint p poly = isNothing $ find f $ floorPlanPoints poly
    where f v = dist (getPos v) (p ^. _position) < 2.0

-- | function to show an ObjectAdder to add a new roof surface
addSurface :: forall e. SurfaceBuilderCfg -> Node e (Event RoofSurface)
addSurface cfg = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        actDyn  = isActive <$> cfg ^. _modeDyn
        pntsEvt = performEvent $ getCandPoint <$> gateDyn actDyn (cfg ^. _mouseMove)

        f s (Just p) = if validCandPoint p (s ^. _polygon) then Just p else Nothing
        f _ Nothing  = Nothing
        
        candPntDyn = step Nothing $ f <$> (dynEvent $ cfg ^. _floor) <*> pntsEvt

        toSurf = surfaceAround <<< view _position

        opt = def # _name .~ "surface-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
    selPntEvt <- node opt (createObjectAdder candPntDyn actDyn)
    pure $ performEvent $ toSurf <$> selPntEvt


-- | Add/Remove/Update all roof surfaces

newtype SurfaceBuilderCfg = SurfaceBuilderCfg {
    floor     :: Dynamic FloorPlan,
    modeDyn   :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeSurfaceBuilderCfg :: Newtype SurfaceBuilderCfg _
instance defaultSurfaceBuilderCfg :: Default SurfaceBuilderCfg where
    def = SurfaceBuilderCfg {
        floor     : pure def,
        modeDyn   : pure Inactive,
        mouseMove : empty
        }


newtype BuilderState = BuilderState {
    surfaces      :: UUIDMap RoofSurface,
    surfsToRender :: Maybe (UUIDMap RoofSurface)
    }

derive instance newtypeBuilderState :: Newtype BuilderState _
instance defaultBuilderState :: Default BuilderState where
    def = BuilderState {
        surfaces      : M.empty,
        surfsToRender : Nothing
        }

_surfaces :: forall t a r. Newtype t { surfaces :: a | r } => Lens' t a
_surfaces = _Newtype <<< prop (SProxy :: SProxy "surfaces")

_surfsToRender :: forall t a r. Newtype t { surfsToRender :: a | r } => Lens' t a
_surfsToRender = _Newtype <<< prop (SProxy :: SProxy "surfsToRender")


data BuilderOp = AddSurface RoofSurface
               | DelSurface UUID
               | UpdateSurface RoofSurface

derive instance genericBuilderOp :: Generic BuilderOp _
instance showBuilderOp :: Show BuilderOp where
    show = genericShow

renderAll :: BuilderState -> BuilderState
renderAll st = st # _surfsToRender .~ Just (st ^. _surfaces)

applyOp :: BuilderOp -> BuilderState -> BuilderState
applyOp (AddSurface surf)    s = renderAll $ s # _surfaces %~ M.insert (surf ^. idLens) surf
applyOp (DelSurface sid)     s = renderAll $ s # _surfaces %~ M.delete sid
applyOp (UpdateSurface surf) s = s # _surfaces %~ M.update (const $ Just surf) (surf ^. idLens)
                                   # _surfsToRender .~ Nothing


editSurfaces :: forall e. SurfaceBuilderCfg -> Node e (Event (List RoofSurface))
editSurfaces cfg = fixNodeE \stEvt -> do
    -- render all roof surface editors
    let surfsToR = compact $ view _surfsToRender <$> stEvt
    ses <- eventNode $ traverse editRoofSurface <$> surfsToR

    let -- get the delete roof surface event
        delEvt = keepLatest $ foldEvtWith (view _delete) <$> ses
        -- get the update roof surface event
        updEvt = keepLatest $ foldEvtWith (view _surface) <$> ses

    -- render surface adder to add new surfaces
    newSurfEvt <- addSurface cfg

    let opEvt = (AddSurface <$> newSurfEvt) <|>
                (DelSurface <$> delEvt)     <|>
                (UpdateSurface <$> updEvt)

        -- apply operation onto the builder state
        newStEvt = fold applyOp opEvt def

    pure { input : newStEvt, output : empty }
