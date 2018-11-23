module UI.Icon exposing(..)

import Element
import Html
import Html.Attributes

icon : String -> List (Element.Attribute msg) -> Element.Element msg
icon iconName attributes =
  Element.el attributes <|
    Element.html <|
      Html.i [Html.Attributes.class ("fas fa-" ++ iconName)] []
