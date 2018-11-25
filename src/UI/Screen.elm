module UI.Screen exposing (..)

import Element exposing (..)
import Html
import Html.Attributes

screen : Bool -> Element msg -> Element msg
screen = screenWith {options = []}

screenWith : {options : List Option} -> Bool -> Element msg -> Element msg
screenWith options visible elem =
  let
    fixedAttrs = [Html.Attributes.style "position" "fixed", Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%"]
    displayAttrs =  if visible then fixedAttrs
                    else fixedAttrs ++ [Html.Attributes.style "display" "none"]
  in
  html <| Html.div displayAttrs [layoutWith {options = [noStaticStyleSheet] ++ options.options} [width fill, height fill] elem]
