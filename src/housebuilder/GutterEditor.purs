module HouseBuilder.GutterEditor where

import Prelude

import Data.Function.Memoize (memoize)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
import Model.HouseBuilder.Ridge (Ridge, RidgeType(..), mkRidge)
import Model.HouseEditor.HousePoint (gutterPoint)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
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

data GutterPointType = GPPredicted  -- a predicted point that might be used for next gutter point
                     | GPLocked     -- an already locked point
                     | GPReused     -- a locked point that user might want to reuse

derive instance genericGutterPointType :: Generic GutterPointType _
derive instance eqGutterPointType :: Eq GutterPointType
instance showGutterPointType :: Show GutterPointType where
    show = genericShow

newtype GutterPoint = GutterPoint {
    pointType :: GutterPointType,
    position  :: Vector3
}

derive instance newtypeGutterPoint :: Newtype GutterPoint _
derive instance genericGutterPoint :: Generic GutterPoint _
instance showGutterPoint :: Show GutterPoint where
    show = genericShow

_pointType :: forall t a r. Newtype t { pointType :: a | r } => Lens' t a
_pointType = _Newtype <<< prop (SProxy :: SProxy "pointType")

mkGutterPoint :: GutterPointType -> Vector3 -> GutterPoint
mkGutterPoint t p = GutterPoint { pointType : t, position : p }

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
data GEOperation = GEOLockPoint          -- convert predicted point to locked point
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
mkGutter p1 p2 = mkRidge Gutter hp1 hp2
    where hp1 = gutterPoint $ p1 ^. _position
          hp2 = gutterPoint $ p2 ^. _position

-- predict the next gutter point to use based on the current mouse position
predictPoint :: GEState -> Vector3 -> GEState
predictPoint st p = case st ^. _lastLockedPoint of
    Nothing -> st # _predictedPoint .~ Just (mkGutterPoint GPPredicted p)
    Just lp -> predictFrom st lp p

-- predict the next point based on the mouse position, find the best gutter
-- line from the last checked point.
predictFrom :: GEState -> GutterPoint -> Vector3 -> GEState
predictFrom st lp p = let np = fromMaybe p $ alignPredGtter (st ^. _lockedGutters) (lp ^. _position) p
                          
    

-- materials used in gutter editor
loadMat :: Int -> MeshBasicMaterial
loadMat = memoize (\clr -> unsafePerformEffect $ mkMeshBasicMaterial clr)

greenMat :: MeshBasicMaterial
greenMat = loadMat 0x00ff00

blueMat :: MeshBasicMaterial
blueMat = loadMat 0x0000ff

getMarkerGeo :: Unit -> CircleGeometry
getMarkerGeo = memoize (\_ -> unsafePerformEffect $ mkCircleGeometry 0.6 32)

