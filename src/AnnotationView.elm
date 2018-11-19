module AnnotationView exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict

import Utils

import Iiif.Types exposing(..)

annotationView : Maybe Annotation -> Html msg
annotationView maybeAnnotation = 
  case maybeAnnotation of
    Just annotation -> 
      let 
        content = annotation.resource.chars 
                    |> Maybe.withDefault ""
                    |> Utils.sanitiseHtml
      in div [class "card card-body annotation_view"] content
    Nothing -> div [class "card card-body annotation_view hide"] []
