module UI.Icon exposing(..)

import UI.Core exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

icon : String -> List (Attribute msg) -> Html msg
icon iconName attributes =
  el attributes <|
      Html.i [Attributes.class ("fas fa-" ++ iconName)] []
