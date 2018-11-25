module IiifUI.CanvasButton exposing(..)

import Dict

import Element exposing(..)
import Element.Input as Input
import Element.Border as Border

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

canvasButton : CanvasButtonConfig msg -> Element msg
canvasButton config =
  case config.canvas of
    Nothing -> Element.none
    Just canvas_ -> 
      if config.includeLabel then
        column ([spacing 5, width shrink] ++ config.attributes) [canvasButtonOnly config, canvasLabelOnly config]
      else Element.el config.attributes <| canvasButtonOnly config

canvasButtonOnly : CanvasButtonConfig msg -> Element msg
canvasButtonOnly config =
  case config.canvas of
    Nothing -> Element.none
    Just canvas_ -> 
      let
        width_ = round ((toFloat canvas_.width) / (toFloat canvas_.height) * 60.0)
        thumbSrc = Iiif.ImageApi.canvasThumbnailUrl (Iiif.ImageApi.FitH 60) canvas_
        canvasThumb = lazyloadImage [height <| px 60] {src = thumbSrc, spinnerSrc = "spinner_40x60.gif", description = Maybe.withDefault "" canvas_.label}
        selectedAttrs = if config.selected  then [Border.shadow { offset = (0.0, 0.0), size = 2.0, blur = 3.0, color = Colors.primary |> C.scaleAlpha 0.7}]
                                            else []
      in
      Element.el ([width <| px width_, height <| px 60, centerX] ++ selectedAttrs) <| Input.button [] {onPress = config.onPress, label = canvasThumb}

canvasLabelOnly : CanvasButtonConfig msg -> Element msg
canvasLabelOnly config = 
  config.canvas |> Maybe.andThen .label |> Maybe.withDefault "" 
    |> Element.text 
    |> Element.el ([centerX] ++ Fonts.textBody)
