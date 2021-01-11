module HouseBuilder.RoofSurfaceBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_face, _mouseMove, _name, _point, _polygon, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.RoofManager (foldEvtWith)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event, fold, keepLatest)
import FRP.Event.Extra (debug, performEvent)
import Model.ActiveMode (ActiveMode(..))
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseBuilder.RoofSurface (RoofSurface, newSurface, surfaceAround)
import Model.UUID (idLens)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeE, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (mkVec3)


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
    let cfg = def # _isActive .~ pure true
                  # _polygon  .~ rs ^. _polygon
                  
    editor <- createPolyEditor cfg
    
    pure $ def # _surface .~ (performEvent $ newSurface <$> editor ^. _polygon)
               # _delete  .~ (const (rs ^. idLens) <$> editor ^. _delete)

-- | function to show an ObjectAdder to add a new roof surface
addSurface :: forall e. SurfaceBuilderCfg -> Dynamic Boolean -> Node e (Event RoofSurface)
addSurface cfg actDyn = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        pntsEvt = performEvent $ getCandPoint <$> gateDyn actDyn (cfg ^. _mouseMove)
        candPntDyn = step Nothing pntsEvt

        toSurf = surfaceAround <<< view _position

        opt = def # _name .~ "surface-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
    selPntEvt <- node opt (createObjectAdder candPntDyn actDyn)
    pure $ performEvent $ toSurf <$> selPntEvt


-- | Add/Remove/Update all roof surfaces

newtype SurfaceBuilderCfg = SurfaceBuilderCfg {
    floor     :: FloorPlan,
    modeDyn   :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeSurfaceBuilderCfg :: Newtype SurfaceBuilderCfg _
instance defaultSurfaceBuilderCfg :: Default SurfaceBuilderCfg where
    def = SurfaceBuilderCfg {
        floor     : def,
        modeDyn   : pure Inactive,
        mouseMove : empty
        }


newtype BuilderState = BuilderState {
    surfaces :: UUIDMap RoofSurface
    }

derive instance newtypeBuilderState :: Newtype BuilderState _
instance defaultBuilderState :: Default BuilderState where
    def = BuilderState {
        surfaces : M.empty
        }

_surfaces :: forall t a r. Newtype t { surfaces :: a | r } => Lens' t a
_surfaces = _Newtype <<< prop (SProxy :: SProxy "surfaces")


data BuilderOp = AddSurface RoofSurface
               | DelSurface UUID
               | UpdateSurface RoofSurface

derive instance genericBuilderOp :: Generic BuilderOp _
instance showBuilderOp :: Show BuilderOp where
    show = genericShow


applyOp :: BuilderOp -> BuilderState -> BuilderState
applyOp (AddSurface surf)    s = s # _surfaces %~ M.insert (surf ^. idLens) surf
applyOp (DelSurface sid)     s = s # _surfaces %~ M.delete sid
applyOp (UpdateSurface surf) s = s # _surfaces %~ M.update (const $ Just surf) (surf ^. idLens)


editSurfaces :: forall e. SurfaceBuilderCfg -> Dynamic Boolean -> Node e (Event (List RoofSurface))
editSurfaces cfg actDyn = fixNodeE \stEvt -> do
    -- render all roof surface editors
    ses <- eventNode $ (traverse editRoofSurface <<< view _surfaces) <$> stEvt

    let -- get the delete roof surface event
        delEvt = keepLatest $ foldEvtWith (view _delete) <$> ses
        -- get the update roof surface event
        updEvt = keepLatest $ foldEvtWith (view _surface) <$> ses

    -- render surface adder to add new surfaces
    newSurfEvt <- addSurface cfg actDyn

    let opEvt = (AddSurface <$> newSurfEvt) <|>
                (DelSurface <$> delEvt)     <|>
                (UpdateSurface <$> updEvt)

        -- apply operation onto the builder state
        newStEvt = fold applyOp (debug opEvt) def

    pure { input : newStEvt, output : empty }
