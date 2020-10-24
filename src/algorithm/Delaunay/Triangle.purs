module Algorithm.Delaunay.Triangle where

foreign import data Triangle :: Type -> Type
foreign import mkTriangle :: forall v. v -> v -> v -> Triangle v

foreign import triVertex1 :: forall v. Triangle v -> v
foreign import triVertex2 :: forall v. Triangle v -> v
foreign import triVertex3 :: forall v. Triangle v -> v
