module UI.LazyloadImage exposing (..)

import Element exposing(..)
import Element.Keyed as Keyed
import Html
import Html.Attributes

lazyloadImage : List (Attribute msg) -> { src : String, spinnerSrc : String, description : String} -> Element msg
lazyloadImage attrs config =
  let
    imgAttrs =
      [ Html.Attributes.class "lazyload"
      , Html.Attributes.attribute "data-src" config.src
      , Html.Attributes.src config.spinnerSrc
      , Html.Attributes.alt config.description
      ]
  in
  Keyed.el attrs (config.src, Element.html <| Html.img imgAttrs [])
