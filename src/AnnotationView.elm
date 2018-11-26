module AnnotationView exposing(..)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import UI.Core exposing(..)
import UI.Panel as Panel
import UI.Colors as Colors

import Dict

import Utils

import Iiif.Types exposing(..)

annotationView : List (Attribute msg) -> Maybe Annotation -> Html msg
annotationView attrs maybeAnnotation = 
  case maybeAnnotation of
    Just annotation -> 
      let 
        content = annotation.resource.chars 
                    |> Maybe.withDefault ""
                    |> Utils.sanitiseHtml
      in 
        Panel.white
          |> Panel.attributes attrs
          |> Panel.content (div [fullWidth, Attributes.style "overflow-wrap" "break-word"] content) 
          |> Panel.popup 
          |> Panel.attributes [cssBackgroundColor <| Colors.toCss Colors.defaultBackground]
          |> Panel.panel
    Nothing -> none
