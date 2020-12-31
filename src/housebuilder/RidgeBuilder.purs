module HouseBuilder.RidgeBuilder where


import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), delete)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_deleted, _dragging, _face, _mouseMove, _name, _point, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.RoofManager (foldEvtWith)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event, fold, keepLatest)
import FRP.Event.Extra (distinct, multicast, performEvent)
import HouseBuilder.Rendering.HousePoint (RidgePointRendered, _dragEnd)
import Model.ActiveMode (ActiveMode)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseBuilder.House (_ridges)
import Model.HouseBuilder.Ridge (Ridge, topRidge)
import Model.HouseEditor.HousePoint (RidgePoint, ridgePoint)
import Rendering.DynamicNode (renderEvent)
import Rendering.Node (Node, fixNodeE, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, mkVec3)


-- | let user add new ridge point
addRidgePoint :: forall e. RidgeEditorConf -> Dynamic Boolean -> Node e (Event RidgePoint)
addRidgePoint cfg canShowDyn = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        pntsEvt      = performEvent $ getCandPoint <$> gateDyn canShowDyn (cfg ^. _mouseMove)
        candPntDyn   = step Nothing pntsEvt

        toRidgePoint = ridgePoint <<< view _position
        
        opt = def # _name     .~ "ridge-point-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
                  
    map toRidgePoint <$> node opt (createObjectAdder candPntDyn canShowDyn)


newtype RidgeEditorState = RidgeEditorState {
    ridgePoints :: List RidgePoint,
    ridges      :: List Ridge,
    tempRidge   :: Maybe Ridge
    }

derive instance newtypeRidgeEditorState :: Newtype RidgeEditorState _
instance defaultRidgeEditorState :: Default RidgeEditorState where
    def = RidgeEditorState {
        ridgePoints : Nil,
        ridges      : Nil,
        tempRidge   : Nothing
        }

_ridgePoints :: forall t a r. Newtype t { ridgePoints :: a | r } => Lens' t a
_ridgePoints = _Newtype <<< prop (SProxy :: SProxy "ridgePoints")

_tempRidge :: forall t a r. Newtype t { tempRidge :: a | r } => Lens' t a
_tempRidge = _Newtype <<< prop (SProxy :: SProxy "tempRidge")

data RidgeOp = ROAddRidgePoint RidgePoint
             | RODelRidgePoint RidgePoint
             | RODragRidgePoint RidgePoint RidgePoint
             | ROAddRidge


applyOp :: RidgeOp -> RidgeEditorState -> RidgeEditorState
applyOp (ROAddRidgePoint p)     st = st # _ridgePoints %~ Cons p
applyOp (RODelRidgePoint p)     st = st # _ridgePoints %~ delete p
applyOp (RODragRidgePoint p np) st = st # _tempRidge .~ Just (topRidge p np)
applyOp ROAddRidge              st = case st ^. _tempRidge of
    Just r -> st # _ridges %~ Cons r
                 # _tempRidge .~ Nothing
    Nothing -> st


newtype RidgeEditorConf = RidgeEditorConf {
    floor     :: FloorPlan,
    mode      :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeRidgeEditorConf :: Newtype RidgeEditorConf _


dragOp :: Tuple RidgePoint Vector3 -> RidgeOp
dragOp (Tuple r v) = RODragRidgePoint r (ridgePoint v)

editRidges :: forall e. RidgeEditorConf -> Node e (Event Unit)
editRidges conf = fixNodeE \opEvt -> do
    let stEvt = fold applyOp opEvt def

        rpsEvt = distinct $ view _ridgePoints <$> stEvt
    rpNodesEvt :: Event (List RidgePointRendered) <- multicast <$> renderEvent rpsEvt

    -- let user add new ridge points
    addedPntEvt <- map ROAddRidgePoint <$> addRidgePoint conf (pure true)

    let -- delete ridge point event
        delPntEvt = map RODelRidgePoint $ keepLatest $ foldEvtWith (view _deleted) <$> rpNodesEvt
        -- drag ridge point to add a new ridge
        dragEvt = map dragOp $ keepLatest $ foldEvtWith (view _dragging) <$> rpNodesEvt
        -- drag ended and add the temp ridge to be real ridge
        addRidgeEvt = map (const ROAddRidge) $ keepLatest $ foldEvtWith (view _dragEnd) <$> rpNodesEvt

        newOpEvt = addedPntEvt <|> delPntEvt <|> dragEvt <|> addRidgeEvt
        
    pure { input : newOpEvt, output : empty }
