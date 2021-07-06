module Troika.Text where

import Prelude

import Editor.Disposable (class Disposable)
import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Text :: Type

foreign import mkText       :: Effect Text
foreign import setText      :: String -> Text -> Effect Unit
foreign import setFontSize  :: Number -> Text -> Effect Unit
foreign import setColor     :: Int    -> Text -> Effect Unit
foreign import setTextAlign :: String -> Text -> Effect Unit
foreign import sync         :: Text   -> Effect Unit
foreign import dispose      :: Text   -> Effect Unit

instance isObject3DText :: IsObject3D Text where
    toObject3D = unsafeCoerce

instance disposableText :: Disposable Text where
    dispose = dispose
