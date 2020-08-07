module IiifUI.IiifLink exposing(..)

import UI.Core exposing(..)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import Iiif.Types

iiifIcon : Html msg
iiifIcon = div [Attributes.style "height" "18px", Attributes.style "width" "21px", Attributes.class "iiif_logo"] []

iiifLink : Iiif.Types.Uri -> Html msg
iiifLink uri =
  a [Attributes.href uri, Attributes.target "_blank"] [iiifIcon]
