module UI.Icon exposing(IconStyle(..), icon, icon2)

import UI.Core exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

type IconStyle = Regular | Solid

icon : String -> List (Attribute msg) -> Html msg
icon = icon2 Solid

icon2 : IconStyle -> String -> List (Attribute msg) -> Html msg
icon2 style iconName attributes =
  el attributes <|
      Html.i [Attributes.class ((iconStyleClass style) ++ " fa-" ++ iconName)] []

iconStyleClass : IconStyle -> String
iconStyleClass style =
  case style of
    Regular -> "far"
    Solid -> "fas"