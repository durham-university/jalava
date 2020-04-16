module UI.Button exposing(..)

import UI.Fonts exposing(..)
import UI.Colors as Colors
import UI.ColorUtils as C
import UI.Core exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

type alias ButtonConfig msg =
  { baseColor : String
  , content : Html msg
  , round : Int
  , padding : Int
  , attributes : List (Attribute msg)
  , onPress : Maybe msg
  , linkStyle : Bool
  , title : Maybe String
  }

emptyConfig : ButtonConfig msg
emptyConfig =
  { baseColor = "Primary"
  , content = UI.Core.none
  , round = 10
  , padding = 10
  , attributes = []
  , onPress = Nothing
  , linkStyle = False
  , title = Nothing
  }

button : ButtonConfig msg -> Html msg
button config = 
  let
    colourStyle = 
      if config.linkStyle then
        [ Attributes.class "textHover"
        , Attributes.class ("text" ++ config.baseColor)
        ]
      else 
        [ cssColor <| Colors.toCss <| C.rgb 1.0 1.0 1.0
        , Attributes.class "bgHover"
        , Attributes.class ("bg" ++ config.baseColor)
        ]
    clickAttribute = Maybe.map (List.singleton << Events.onClick) config.onPress |> Maybe.withDefault []
    titleAttribute = Maybe.map (List.singleton << Attributes.title) config.title |> Maybe.withDefault []
  in
  UI.Core.button
    (textBody ++ colourStyle ++ 
      [ cssPadding <| cssPx config.padding
      , cssBorderRadius <| cssPx config.round
      , Attributes.style "cursor" "pointer"
      , Attributes.style "justify-content" "center"
      , Attributes.style "align-items" "center"
      ] 
    ++ clickAttribute ++ titleAttribute ++ config.attributes)
    config.content

title : String -> ButtonConfig msg -> ButtonConfig msg
title s config = {config | title = Just s}

attributes : List (Attribute msg) -> ButtonConfig msg -> ButtonConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> ButtonConfig msg -> ButtonConfig msg
color color_ config = {config | baseColor = color_}

onPress : msg -> ButtonConfig msg -> ButtonConfig msg
onPress msg_ config = {config | onPress = Just msg_}

maybeOnPress : Maybe msg -> ButtonConfig msg -> ButtonConfig msg
maybeOnPress msg_ config = {config | onPress = msg_}

content : Html msg -> ButtonConfig msg -> ButtonConfig msg
content content_ config = {config | content = content_}

round : Int -> ButtonConfig msg -> ButtonConfig msg
round value config = {config | round = value}

padding : Int -> ButtonConfig msg -> ButtonConfig msg
padding value config = {config | padding = value}

linkStyle : ButtonConfig msg -> ButtonConfig msg
linkStyle config = {config | linkStyle = True}


primary : ButtonConfig msg
primary = emptyConfig |> color "Primary"

secondary : ButtonConfig msg
secondary = emptyConfig |> color "Secondary"

danger : ButtonConfig msg
danger = emptyConfig |> color "Error"

light : ButtonConfig msg
light = emptyConfig
          |> color "Light"
          |> attributes [cssColor <| Colors.toCss Colors.defaultTextColor]

dark : ButtonConfig msg
dark = emptyConfig |> color "Dark"

link : ButtonConfig msg
link = emptyConfig
          |> color "Link"
          |> linkStyle

slimLink : ButtonConfig msg
slimLink = emptyConfig
              |> color "Link"
              |> linkStyle
              |> padding 0
              |> round 0
