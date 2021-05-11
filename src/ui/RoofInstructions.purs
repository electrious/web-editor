module UI.RoofInstructions where

import Prelude hiding (div)

import Specular.Dom.Builder.Class (text)
import Specular.Dom.Element (attrs, classes, el)
import Specular.Dom.Widget (Widget, emptyWidget)
import UI.Utils (Style, div, mkStyle, (:~))

dotStyle :: Array Style
dotStyle = ["height"        :~ "25px",
            "width"         :~ "25px",
            "border-radius" :~ "50%",
            "display"       :~ "inline-block"
           ]
red :: Array Style
red = ["background-color" :~ "red"]

green :: Array Style
green = ["background-color" :~ "green"]

blue :: Array Style
blue = ["background-color" :~ "blue"]

orange :: Array Style
orange = ["background-color" :~ "orange"]


roofInstructions :: Widget Unit
roofInstructions =
    el "table" [classes ["uk-table", "uk-table-small"]] $
        el "tbody" [] do
            el "tr" [] do
                el "td" [] $ div [attrs $ mkStyle $ dotStyle <> blue] emptyWidget
                el "td" [] $ text "Click to create a new roofplate"
            el "tr" [] do
                el "td" [] $ div [attrs $ mkStyle $ dotStyle <> orange] emptyWidget
                el "td" [] $ text "Click to delete the roofplate"
            el "tr" [] do
                el "td" [] $ div [attrs $ mkStyle $ dotStyle <> red] emptyWidget
                el "td" [] $ text "Click to delete a vertex. Drag to move the vertex"
            el "tr" [] do
                el "td" [] $ div [attrs $ mkStyle $ dotStyle <> green] emptyWidget
                el "td" [] $ text "Click to add a new vertex"
            el "tr" [] do
                el "td" [] $ el "b" [] $ text "Important:"
                el "td" [] do
                    text "This project is still in Beta. If your new roof plate layout"
                    el "br" [] emptyWidget
                    text "is not being reflected in App after 2 minutes, please delete all"
                    el "br" [] emptyWidget
                    text "roof plates and start from scratch."
