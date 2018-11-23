module UI.Toast exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Icon as Icon
import UI.Button as Button
import UI.TitleLine as TitleLine
import UI.Fonts exposing(..)

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input

type alias ToastConfig msg =
  { content: Element msg
  , round: Int
  , onClose: Maybe msg
  , baseColor: Color
  , attributes: List (Attribute msg)
  }

emptyConfig : ToastConfig msg
emptyConfig =
  { content = Element.none
  , round = 5
  , onClose = Nothing
  , baseColor = Colors.primary
  , attributes = []
  }

toast : ToastConfig msg -> Element msg
toast config = 
  Element.row
    ( textBody ++ 
      [ Element.width Element.fill
      , Element.padding 15
      , Border.rounded config.round
      , Border.color config.baseColor
      , Border.width 1
      , Font.color <| C.darken 0.5 config.baseColor
      , Background.color <| C.desaturate 0.7 config.baseColor
      , spacing 5
      ]
      ++ config.attributes )
    [ config.content
    , Button.slimLink |> Button.attributes [Element.alignRight] |> Button.color config.baseColor |> Button.content (TitleLine.iconOnly "times") |> Button.button
--    , Input.button [Element.alignRight] {onPress = config.onClose, label = Icon.icon "times" []}
    ]

attributes : List (Attribute msg) -> ToastConfig msg -> ToastConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : Color -> ToastConfig msg -> ToastConfig msg
color color_ config = {config | baseColor = color_}

onClose : msg -> ToastConfig msg -> ToastConfig msg
onClose msg_ config = {config | onClose = Just msg_}

content : Element msg -> ToastConfig msg -> ToastConfig msg
content content_ config = {config | content = content_}

round : Int -> ToastConfig msg -> ToastConfig msg
round amount config = {config | round = amount}


primary : ToastConfig msg
primary = emptyConfig |> color Colors.primary

secondary : ToastConfig msg
secondary = emptyConfig |> color Colors.secondary

success : ToastConfig msg
success = emptyConfig |> color Colors.success

error : ToastConfig msg
error = emptyConfig |> color Colors.error

warning : ToastConfig msg
warning = emptyConfig |> color Colors.warning

white : ToastConfig msg
white = 
  emptyConfig 
    |> color (rgb 0.5 0.5 0.5)
    |> attributes 
        [ Background.color <| rgb 1.0 1.0 1.0
        , Font.color Colors.defaultTextColor
        ]