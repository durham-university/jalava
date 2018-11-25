module IiifUI.IiifLink exposing(..)

import Element exposing(Element, width, height, px)

import Iiif.Types

iiifLink : Iiif.Types.Uri -> Element msg
iiifLink uri =
  Element.link [] { url = uri, label = Element.image [height <| px 18, width <| px 21] { src = "logo-iiif-small.png", description = "IIIF"} }
