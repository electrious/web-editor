module UI.Utils where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object as O
import Specular.Dom.Browser (Attrs, (:=))
import Specular.Dom.Element (Prop, attrs, onClick_, text)
import Specular.Dom.Element.Class (el, el_)
import Specular.Dom.Widget (RWidget)
import Specular.FRP (Event, newEvent)

-- | helper function to create a div element
div :: forall r a. Array Prop -> RWidget r a -> RWidget r a
div = el "div"

div_ :: forall r. Array Prop -> RWidget r Unit
div_ = el_ "div"

div' :: forall r a. RWidget r a -> RWidget r a
div' = el "div" []

span :: forall r a. Array Prop -> RWidget r a -> RWidget r a
span = el "span"

span_ :: forall r. Array Prop -> RWidget r Unit
span_ = el_ "span"

span' :: forall r a. RWidget r a -> RWidget r a
span' = el "span" []

-- | helper function to create a p element
p :: forall r a. Array Prop -> RWidget r a -> RWidget r a
p = el "p"

type URL = String

-- | helper function to create a <a> element
elA :: forall r. String -> URL -> RWidget r (Event Unit)
elA t url = do
    { event: e, fire: pushF } <- liftEffect newEvent
    el "a" [attrs $ mkAttrs ["href" :~ url], onClick_ (pushF unit)] $ text t
    pure e

data Style = Style String String

style :: String -> String -> Style
style = Style

infixl 9 style as :~

mkStyle :: forall f. Functor f => Foldable f => f Style -> Attrs
mkStyle ps = "style" := joinWith "; " (fromFoldable $ f <$> ps)
    where f (Style k v) = k <> ": " <> v

mkAttrs :: forall f. Functor f => Foldable f => f Style -> Attrs
mkAttrs = O.fromFoldable <<< map f
    where f (Style k v) = Tuple k v
