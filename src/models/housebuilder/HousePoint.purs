module Model.HouseEditor.HousePoint where


import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Three.Math.Vector (Vector3)


newtype RidgePoint = RidgePoint Vector3

derive instance newtypeRidgePoint :: Newtype RidgePoint _

ridgePoint :: Vector3 -> RidgePoint
ridgePoint = RidgePoint

_ridgePointPos :: Lens' RidgePoint Vector3
_ridgePointPos = _Newtype


newtype GutterPoint = GutterPoint Vector3

derive instance newtypeGutterPoint :: Newtype GutterPoint _

gutterPoint :: Vector3 -> GutterPoint
gutterPoint = GutterPoint

_gutterPointPos :: Lens' GutterPoint Vector3
_gutterPointPos = _Newtype
