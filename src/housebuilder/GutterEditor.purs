module HouseBuilder.GutterEditor where

import Prelude hiding (degree)

import Data.Filterable (filter)
import Data.Function.Memoize (memoize)
import Data.Lens (Lens', view, (^.), (%~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_object, _position)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Math.Angle (degree)
import Math.Line (linesAngle, mkLine, mostParaLine, projPointWithLine)
import Model.HouseBuilder.Ridge (Ridge, RidgeType(..), mkRidge, ridgeLine)
import Model.HouseEditor.HousePoint (GutterPoint, GutterPointType(..), HousePoint(..), _pointType, gutterPoint)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Object3D (class IsObject3D, Object3D)
import Three.Math.Vector (Vector3)

newtype GutterEditor = GutterEditor {
    object  :: Object3D,
    gutters :: Event (Array Ridge)
}

derive instance newtypeGutterEditor :: Newtype GutterEditor _
instance isObject3DGutterEditor :: IsObject3D GutterEditor where
    toObject3D = view _object

_gutters :: forall t a r. Newtype t { gutters :: a | r } => Lens' t a
_gutters = _Newtype <<< prop (SProxy :: SProxy "gutters")


-- GutterEditor State
newtype GEState = GEState {
    lockedPoints    :: List GutterPoint,
    lockedGutters   :: List Ridge,
    lastLockedPoint :: Maybe GutterPoint,
    predictedPoint  :: Maybe GutterPoint,
    predictedGutter :: Maybe Ridge
}

derive instance newtypeGEState :: Newtype GEState _

_lockedPoints :: forall t a r. Newtype t { lockedPoints :: a | r } => Lens' t a
_lockedPoints = _Newtype <<< prop (SProxy :: SProxy "lockedPoints")

_lockedGutters :: forall t a r. Newtype t { lockedGutters :: a | r } => Lens' t a
_lockedGutters = _Newtype <<< prop (SProxy :: SProxy "lockedGutters")

_lastLockedPoint :: forall t a r. Newtype t { lastLockedPoint :: a | r } => Lens' t a
_lastLockedPoint = _Newtype <<< prop (SProxy :: SProxy "lastLockedPoint")

_predictedPoint :: forall t a r. Newtype t { predictedPoint :: a | r } => Lens' t a
_predictedPoint = _Newtype <<< prop (SProxy :: SProxy "predictedPoint")

_predictedGutter :: forall t a r. Newtype t { predictedGutter :: a | r } => Lens' t a
_predictedGutter = _Newtype <<< prop (SProxy :: SProxy "predictedGutter")

-- gutter editor operations
data GEOperation = GEOLockPoint             -- convert predicted point to locked point
                 | GEOPredictPoint Vector3  -- predict the next point to use based on current mouse position

-- update editor state with operations
applyOp :: GEState -> GEOperation -> GEState
applyOp st GEOLockPoint        = lockNewPoint st
applyOp st (GEOPredictPoint p) = predictPoint st p

-- lock the predicted point if it exists.
lockNewPoint :: GEState -> GEState
lockNewPoint st = case st ^. _predictedPoint of
    Nothing -> st
    Just pp -> let np = pp # _pointType .~ GPLocked
                   f Nothing  l = l
                   f (Just v) l = v : l
               in st # _lastLockedPoint .~ Just np
                     # _lockedPoints    %~ ((:) np)
                     # _lockedGutters   %~ (f (st ^. _predictedGutter))
                     # _predictedPoint  .~ Nothing
                     # _predictedGutter .~ Nothing

mkGutter :: GutterPoint -> GutterPoint -> Ridge
mkGutter p1 p2 = mkRidge Gutter (HousePointGutter hp1) (HousePointGutter hp2)
    where hp1 = gutterPoint GPPredicted $ p1 ^. _position
          hp2 = gutterPoint GPPredicted $ p2 ^. _position

-- predict the next gutter point to use based on the current mouse position
predictPoint :: GEState -> Vector3 -> GEState
predictPoint st p = case st ^. _lastLockedPoint of
    Nothing -> st # _predictedPoint .~ Just (gutterPoint GPPredicted p)
    Just lp -> predictFrom st lp p

-- predict the next point based on the mouse position, find the best gutter
-- line from the last checked point.
predictFrom :: GEState -> GutterPoint -> Vector3 -> GEState
predictFrom st lp p = let np = fromMaybe p $ alignPredGutter (st ^. _lockedGutters) (lp ^. _position) p
                          gtPt = gutterPoint GPPredicted np
                      in st # _predictedPoint  .~ Just gtPt
                            # _predictedGutter .~ Just (mkGutter lp gtPt)


-- align the predicted gutter with existing gutters as much as possible
alignPredGutter :: List Ridge -> Vector3 -> Vector3 -> Maybe Vector3
alignPredGutter gutters lp p =
    let lines = ridgeLine <$> gutters
        tl    = mkLine lp p  -- target line
        -- find the line with smallest angle between it and the target line
        f l   = linesAngle l tl < degree 5.0
    in map (projPointWithLine lp p) $ filter f $ mostParaLine tl lines


getMarkerGeo :: Unit -> CircleGeometry
getMarkerGeo = memoize (\_ -> unsafePerformEffect $ mkCircleGeometry 0.6 32)
