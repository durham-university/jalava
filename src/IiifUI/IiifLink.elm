module IiifUI.IiifLink exposing(..)

import UI.Core exposing(..)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import Iiif.Types

iiifIcon : Html msg
iiifIcon = img [Attributes.height 18, Attributes.width 21, Attributes.src "logo-iiif-small.png", Attributes.alt "IIIF"] []

iiifLink : Iiif.Types.Uri -> Html msg
iiifLink uri =
  a [Attributes.href uri, Attributes.target "_blank"] [iiifIcon]
