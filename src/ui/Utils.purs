module UI.Utils where

import Prelude

import Specular.Dom.Element (Prop)
import Specular.Dom.Element.Class (el, el_)
import Specular.Dom.Widget (RWidget)

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
