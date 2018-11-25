module UI.Tabs exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Fonts exposing(..)

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input

type alias TabsConfig key msg =
  { tabs: List (key, Element msg)
  , onPress: Maybe (key -> msg)
  , selected: Maybe key
  , baseColor: Color
  , paddingH: Int
  , paddingV: Int
  , attributes: List (Attribute msg)
  }

empty : TabsConfig key msg
empty =
  { tabs = []
  , onPress = Nothing
  , selected = Nothing
  , baseColor = Colors.link
  , paddingV = 10
  , paddingH = 15
  , attributes = []
  }

tabs : TabsConfig key msg -> Element msg
tabs config =
  let
    pressHandler : key -> Maybe msg
    pressHandler key =
      case config.onPress of
        Just f -> Just <| f key
        Nothing -> Nothing
    tabButton (key, e) =
      if config.selected == Just key then
        Input.button (textBody ++ 
            [ paddingEach { top = config.paddingV, bottom = config.paddingV + 1, left = config.paddingH, right = config.paddingH }
            , Font.color <| C.darken 0.7 config.baseColor
            , Border.widthEach { bottom = 0, top = 1, left = 1, right = 1}
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0}
            , Border.color Colors.divider
            , focused [Border.shadow { offset=(0.0, 0.0), size=0.0, blur=0.0, color = rgb 0.0 0.0 0.0}]
            ]) {label = e, onPress = pressHandler key}
      else
        Input.button (textBody ++ 
            [ paddingEach { top = config.paddingV + 1, bottom = config.paddingV, left = config.paddingH + 1, right = config.paddingH + 1 }
            , Font.color config.baseColor
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0}
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0}
            , Border.color Colors.divider
            , focused [Border.shadow { offset=(0.0, 0.0), size=0.0, blur=0.0, color = rgb 0.0 0.0 0.0}]
            , mouseOver [ config.baseColor |> C.desaturate 0.9 |> Background.color ]
            ]) {label = e, onPress = pressHandler key}
    filler = 
      el 
        [ width fill
        , alignBottom
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0}
        , Border.color Colors.divider
        ] Element.none
  in
    wrappedRow [width fill] <| (List.map tabButton config.tabs) ++ [filler]

attributes : List (Attribute msg) -> TabsConfig key msg -> TabsConfig key msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : Color -> TabsConfig key msg -> TabsConfig key msg
color color_ config = {config | baseColor = color_}

content : List (key, Element msg) -> TabsConfig key msg -> TabsConfig key msg
content tabs_ config = {config | tabs = tabs_}

addTab : key -> Element msg -> TabsConfig key msg -> TabsConfig key msg
addTab key_ tab_ config = {config | tabs = config.tabs ++ [(key_, tab_)]}

selected : key -> TabsConfig key msg -> TabsConfig key msg
selected key_ config = { config | selected = Just key_ }

maybeSelected : Maybe key -> TabsConfig key msg -> TabsConfig key msg
maybeSelected key_ config = { config | selected = key_ }

onPress : (key -> msg) -> TabsConfig key msg -> TabsConfig key msg
onPress f config = {config | onPress = Just f}

maybeOnPress : Maybe (key -> msg) -> TabsConfig key msg -> TabsConfig key msg
maybeOnPress f config = {config | onPress = f}

default : TabsConfig key msg
default = empty

