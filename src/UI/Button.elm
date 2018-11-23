module UI.Button exposing(..)

import UI.Fonts exposing(..)
import UI.Colors as Colors
import UI.ColorUtils as C

import Element exposing(..)
import Element.Input as Input
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font

type alias ButtonConfig msg =
  { baseColor : Color
  , content : Element msg
  , round : Int
  , padding : Int
  , attributes : List (Attribute msg)
  , onPress : Maybe msg
  , linkStyle : Bool
  }

emptyConfig : ButtonConfig msg
emptyConfig =
  { baseColor = Colors.primary
  , content = Element.none
  , round = 10
  , padding = 10
  , attributes = []
  , onPress = Nothing
  , linkStyle = False
  }

button : ButtonConfig msg -> Element msg
button config = 
  let
    colourAttributes = 
      if config.linkStyle then
        [ Font.color config.baseColor
        , mouseOver [ Font.color <| C.darken 0.3 config.baseColor ]
        ]
      else 
        [ Border.color config.baseColor
        , Font.color <| rgb 1.0 1.0 1.0
        , Background.color config.baseColor
        , mouseOver [ Background.color <| C.darken 0.3 config.baseColor ]
        ]
  in
  Input.button
    (textBody ++ colourAttributes ++
     [ Element.padding config.padding
     , Border.rounded config.round
--     , focused [Border.shadow { offset=(0.0, 0.0), size=2.0, blur=2.0, color = C.desaturate 0.5 config.baseColor }]
    ] ++ config.attributes)
    { onPress = config.onPress, label = config.content}

attributes : List (Attribute msg) -> ButtonConfig msg -> ButtonConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : Color -> ButtonConfig msg -> ButtonConfig msg
color c config = {config | baseColor = c}

content : Element msg -> ButtonConfig msg -> ButtonConfig msg
content c config = {config | content = c}

onPress : msg -> ButtonConfig msg -> ButtonConfig msg
onPress m config = {config | onPress = Just m}

maybeOnPress : Maybe msg -> ButtonConfig msg -> ButtonConfig msg
maybeOnPress m config = {config | onPress = m}

round : Int -> ButtonConfig msg -> ButtonConfig msg
round i config = {config | round = i}

padding : Int -> ButtonConfig msg -> ButtonConfig msg
padding i config = {config | padding = i}

linkStyle : ButtonConfig msg -> ButtonConfig msg
linkStyle config = {config | linkStyle = True}


primary : ButtonConfig msg
primary = emptyConfig |> color Colors.primary

secondary : ButtonConfig msg
secondary = emptyConfig |> color Colors.secondary

danger : ButtonConfig msg
danger = emptyConfig |> color Colors.error

light : ButtonConfig msg
light = emptyConfig
          |> color Colors.lightBg
          |> attributes [Font.color Colors.defaultTextColor]

dark : ButtonConfig msg
dark = emptyConfig |> color (rgb 0.3 0.3 0.3)

link : ButtonConfig msg
link = emptyConfig
          |> color Colors.link
          |> linkStyle

slimLink : ButtonConfig msg
slimLink = emptyConfig
              |> color Colors.link
              |> linkStyle
              |> padding 0
              |> round 0
