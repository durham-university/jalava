module IiifUI.CanvasButton exposing(..)

import Dict

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import UI.Core exposing(..)
import UI.Colors as Colors
import UI.ColorUtils as C
import UI.DefinitionList as DefinitionList
import UI.LazyloadImage exposing(lazyloadImage)
import UI.Fonts as Fonts

import IiifUI.IiifLink as IiifLink

import Utils

import Iiif.Types exposing(..)
import Iiif.ImageApi

type alias CanvasButtonConfig msg =
  { canvas : Maybe Canvas
  , onPress : Maybe msg
  , includeLabel : Bool
  , selected : Bool
  , attributes : List (Attribute msg)
  }

empty : CanvasButtonConfig msg
empty =
  { canvas = Nothing
  , onPress = Nothing
  , includeLabel = False
  , selected = False
  , attributes = []
  }

canvas : Canvas -> CanvasButtonConfig msg -> CanvasButtonConfig msg
canvas canvas_ config = {config | canvas = Just canvas_ }

onPress : msg -> CanvasButtonConfig msg -> CanvasButtonConfig msg
onPress msg config = {config | onPress = Just msg}

includeLabel : CanvasButtonConfig msg -> CanvasButtonConfig msg
includeLabel config = {config | includeLabel = True}

attributes : List (Attribute msg) -> CanvasButtonConfig msg -> CanvasButtonConfig msg
attributes attrs model = {model | attributes = attrs}

selected : Bool -> CanvasButtonConfig msg -> CanvasButtonConfig msg
selected b model = {model | selected = b}

canvasButton : CanvasButtonConfig msg -> Html msg
canvasButton config =
  case config.canvas of
    Nothing -> none
    Just canvas_ -> 
      if config.includeLabel then
        column 5 ([Attributes.style "flex-shrink" <| cssNum 1, Attributes.style "align-items" "center"] ++ config.attributes) [canvasButtonOnly config, canvasLabelOnly config]
      else el config.attributes <| canvasButtonOnly config

canvasButtonOnly : CanvasButtonConfig msg -> Html msg
canvasButtonOnly config =
  case config.canvas of
    Nothing -> none
    Just canvas_ -> 
      let
        width_ = round ((toFloat canvas_.width) / (toFloat canvas_.height) * 60.0)
        thumbSrc = Iiif.ImageApi.canvasThumbnailUrl (Iiif.ImageApi.FitH 60) canvas_
        canvasThumb = lazyloadImage [Attributes.height 60] {src = thumbSrc, spinnerSrc = "spinner_40x60.gif", description = Maybe.withDefault "" canvas_.label}
        selectedAttribute = if config.selected  then [Attributes.style "box-shadow" (String.join " " ["0","0",cssPx 3, cssPx 2, (Colors.primary |> C.scaleAlpha 0.7 |> Colors.toCss)]) ]
                                            else []
        clickAttribute = Maybe.map (List.singleton << Events.onClick) config.onPress |> Maybe.withDefault []
      in
      el ([ cssWidth <| cssPx width_
          , cssHeight <| cssPx 60
          , Attributes.style "cursor" "pointer"
          ] ++ selectedAttribute  ++ clickAttribute) <| UI.Core.button [] canvasThumb
 
canvasLabelOnly : CanvasButtonConfig msg -> Html msg
canvasLabelOnly config = 
  config.canvas |> Maybe.andThen .label |> Maybe.withDefault "" 
    |> text
    |> el (Fonts.textBody ++ [Attributes.style "white-space" "nowrap"])
