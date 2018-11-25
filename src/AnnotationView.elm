module AnnotationView exposing(..)

import Html

import Element exposing(..)
import Element.Background as Background

import UI.Panel as Panel
import UI.Colors as Colors

import Dict

import Utils

import Iiif.Types exposing(..)

annotationView : Maybe Annotation -> Element msg
annotationView maybeAnnotation = 
  case maybeAnnotation of
    Just annotation -> 
      let 
        content = annotation.resource.chars 
                    |> Maybe.withDefault ""
                    |> Utils.sanitiseHtml
      in 
        Panel.empty 
          |> Panel.content (html <| Html.div [] content) 
          |> Panel.popup 
          |> Panel.attributes [Background.color Colors.defaultBackground]
          |> Panel.panel
    Nothing -> Element.none
