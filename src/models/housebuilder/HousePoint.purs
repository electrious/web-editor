module Model.HouseEditor.HousePoint where

import Prelude

import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_position)
import Three.Math.Vector (Vector3)

data GutterPointType = GPPredicted  -- a predicted point that might be used for next gutter point
                     | GPLocked     -- an already locked point
                     | GPReused     -- a locked point that user might want to reuse

derive instance eqGutterPointType :: Eq GutterPointType

newtype GutterPoint = GutterPoint {
    pointType :: GutterPointType,
    position  :: Vector3
    }
derive instance newtypeGutterPoint :: Newtype GutterPoint _
derive instance eqGutterPoint :: Eq GutterPoint

_pointType :: forall t a r. Newtype t { pointType :: a | r } => Lens' t a
_pointType = _Newtype <<< prop (SProxy :: SProxy "pointType")

gutterPoint :: GutterPointType -> Vector3 -> GutterPoint
gutterPoint t p = GutterPoint { pointType : t, position : p }


newtype RidgePoint = RidgePoint {
    position :: Vector3
    }
derive instance newtypeRidgePoint :: Newtype RidgePoint _
derive instance eqwRidgePoint :: Eq RidgePoint

ridgePoint :: Vector3 -> RidgePoint
ridgePoint p = RidgePoint { position : p }

data HousePoint = HousePointGutter GutterPoint
                | HousePointRidge RidgePoint

derive instance eqHousePoint :: Eq HousePoint

pointPos :: HousePoint -> Vector3
pointPos (HousePointGutter p) = p ^. _position
pointPos (HousePointRidge p)  = p ^. _position
