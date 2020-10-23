module Algorithm.Delaunay.Delaunay (triangulate) where

import Algorithm.Delaunay.Triangle (Triangle)

foreign import triangulate :: forall v. Array (Triangle v) -> Array v -> Array (Triangle v)
