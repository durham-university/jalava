module UI.Screen exposing (..)

import UI.Core exposing(..)
import Html exposing(Html)
import Html.Attributes as Attributes

screen : Bool -> Html msg -> Html msg
screen visible elem =
  let
    displayAttrs =  if visible then []
                    else [Attributes.class "hide"]
  in
  Html.div ([Attributes.class "screen"] ++ displayAttrs) [elem]
