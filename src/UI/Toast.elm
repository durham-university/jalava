module UI.Toast exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Icon as Icon
import UI.Button as Button
import UI.TitleLine as TitleLine
import UI.Fonts exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


type alias ToastConfig msg =
  { content: Html msg
  , round: Int
  , onClose: Maybe msg
  , baseColor: String
  , attributes: List (Attribute msg)
  }

emptyConfig : ToastConfig msg
emptyConfig =
  { content = none
  , round = 5
  , onClose = Nothing
  , baseColor = "Primary"
  , attributes = []
  }

toast : ToastConfig msg -> Html msg
toast config = 
  row 5
    ( textBody ++ 
      [ fullWidth
      , cssPadding <| cssPx 15
      , cssBorderRadius <| cssPx config.round
      , Attributes.class ("bg" ++ config.baseColor ++ " bgDesat border" ++ config.baseColor ++ " text" ++ config.baseColor ++ " textDarken")
      , cssBorderWidth <| cssPx 1
      , cssBorderStyle "solid"
      ]
      ++ config.attributes )
    [ el [fullWidth] config.content
    , Button.slimLink 
      |> Button.maybeOnPress config.onClose 
      |> Button.color config.baseColor 
      |> Button.content (TitleLine.iconOnly "times") 
      |> Button.button
    ]

attributes : List (Attribute msg) -> ToastConfig msg -> ToastConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> ToastConfig msg -> ToastConfig msg
color color_ config = {config | baseColor = color_}

onClose : msg -> ToastConfig msg -> ToastConfig msg
onClose msg_ config = {config | onClose = Just msg_}

content : Html msg -> ToastConfig msg -> ToastConfig msg
content content_ config = {config | content = content_}

round : Int -> ToastConfig msg -> ToastConfig msg
round amount config = {config | round = amount}


primary : ToastConfig msg
primary = emptyConfig |> color "Primary"

secondary : ToastConfig msg
secondary = emptyConfig |> color "Secondary"

success : ToastConfig msg
success = emptyConfig |> color "Success"

error : ToastConfig msg
error = emptyConfig |> color "Error"

warning : ToastConfig msg
warning = emptyConfig |> color "Warning"

white : ToastConfig msg
white = 
  emptyConfig 
    |> color "Dim"
    |> attributes 
        [ cssBackgroundColor <| Colors.toCss <| C.rgb 1.0 1.0 1.0
        , cssColor <| Colors.toCss Colors.defaultTextColor
        ]