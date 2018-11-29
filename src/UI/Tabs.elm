module UI.Tabs exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Fonts exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

type alias TabsConfig key msg =
  { tabs: List (key, Html msg)
  , onPress: Maybe (key -> msg)
  , selected: Maybe key
  , baseColor: String
  , paddingH: Int
  , paddingV: Int
  , attributes: List (Attribute msg)
  }

empty : TabsConfig key msg
empty =
  { tabs = []
  , onPress = Nothing
  , selected = Nothing
  , baseColor = "Link"
  , paddingV = 10
  , paddingH = 15
  , attributes = []
  }

tabs : TabsConfig key msg -> Html msg
tabs config =
  let
    pressHandler : key -> Maybe msg
    pressHandler key =
      case config.onPress of
        Just f -> Just <| f key
        Nothing -> Nothing
    tabButton (key, e) =
      let
        clickAttribute = case pressHandler key of
          Just f -> [Events.onClick f]
          Nothing -> []
      in
      if config.selected == Just key then
        UI.Core.button (textBody ++ 
            [ cssPadding4 (cssPx config.paddingV) (cssPx config.paddingH) (cssPx <| config.paddingV + 1) (cssPx config.paddingH)
            , Attributes.class ("text" ++ config.baseColor ++ " textDarken")
            , cssBorderWidth4 (cssPx 1) (cssPx 1) (cssPx 0) (cssPx 1)
            , cssBorderStyle "solid"
            , cssBorderRadius4 (cssPx 5) (cssPx 5) (cssPx 0) (cssPx 0)
            , cssBorderColor <| Colors.toCss Colors.divider
            ] ++ clickAttribute) e
      else
        UI.Core.button ( textBody ++ 
            [ cssPadding4 (cssPx <| config.paddingV + 1) (cssPx <| config.paddingH + 1) (cssPx config.paddingV) (cssPx <| config.paddingH + 1)
            , Attributes.class ("text" ++ config.baseColor ++ " textHover bg" ++ config.baseColor ++" bgHoverOnly bgAlphaHigh")
            , cssBorderWidth4 (cssPx 0) (cssPx 0) (cssPx 1) (cssPx 0)
            , cssBorderStyle "solid"
            , cssBorderRadius4 (cssPx 5) (cssPx 5) (cssPx 0) (cssPx 0)
            , cssBorderColor <| Colors.toCss Colors.divider
            , Attributes.style "cursor" "pointer"
            ] ++ clickAttribute) e
    filler = 
      el 
        [ fullWidth
        , Attributes.style "align-self" "flex-end"
        , cssBorderWidth4 (cssPx 0) (cssPx 0) (cssPx 1) (cssPx 0)
        , cssBorderStyle "solid"
        , cssBorderColor <| Colors.toCss Colors.divider
        ] none
  in
    wrappedRow 0 [fullWidth] <| (List.map tabButton config.tabs) ++ [filler]

attributes : List (Attribute msg) -> TabsConfig key msg -> TabsConfig key msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> TabsConfig key msg -> TabsConfig key msg
color color_ config = {config | baseColor = color_}

content : List (key, Html msg) -> TabsConfig key msg -> TabsConfig key msg
content tabs_ config = {config | tabs = tabs_}

addTab : key -> Html msg -> TabsConfig key msg -> TabsConfig key msg
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

