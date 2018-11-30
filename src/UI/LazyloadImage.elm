module UI.LazyloadImage exposing (..)

import UI.Core exposing(..)
import Html exposing(Attribute, Html, img)
import Html.Attributes as Attributes
import Html.Keyed as Keyed

lazyloadImage : List (Attribute msg) -> { src : String, spinnerSrc : String, description : String} -> Html msg
lazyloadImage attrs config =
  let
    imgAttrs =
      [ Attributes.class "lazyload"
      , Attributes.attribute "data-src" config.src
      , Attributes.src config.spinnerSrc
      , Attributes.alt config.description
      ] ++ attrs
  in
  Keyed.node "div" [] [(config.src, img imgAttrs [])]
