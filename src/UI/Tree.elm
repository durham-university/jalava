module UI.Tree exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Fonts exposing(..)
import UI.Icon exposing(icon)
import UI.Button as Button

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import Json.Decode as Decode


type alias TreeConfig itemType msg =
  { rootItems : List itemType
  , itemChildren : itemType -> List itemType
  , itemOpen : itemType -> Bool
  , itemLabel : itemType -> Html msg
  , itemIcon : itemType -> Maybe (Html msg)
  , itemSelected : itemType -> Bool
  , onPress : itemType -> Maybe msg
  , onPressIcon : itemType -> Maybe msg
  , baseColor : String
  , selectColor : Maybe String
  , padding : Int
  , indent : Int
  , attributes : List (Attribute msg)
  }

empty : TreeConfig itemType msg
empty =
  { rootItems = []
  , itemChildren = \_ -> []
  , itemOpen = \_ -> True
  , itemLabel = \_ -> none
  , itemIcon = \_ -> Nothing
  , itemSelected = \_ -> False
  , onPress = \_ -> Nothing
  , onPressIcon = \_ -> Nothing
  , baseColor = "Primary"
  , selectColor = Nothing
  , padding = 10
  , indent = 20
  , attributes = []
  }

attributes : List (Attribute msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> TreeConfig itemType msg -> TreeConfig itemType msg
color color_ config = {config | baseColor = color_}

selectColor : String -> TreeConfig itemType msg -> TreeConfig itemType msg
selectColor color_ config = {config | selectColor = Just color_}

maybeSelectColor : Maybe String -> TreeConfig itemType msg -> TreeConfig itemType msg
maybeSelectColor color_ config = {config | selectColor = color_}

padding : Int  -> TreeConfig itemType msg -> TreeConfig itemType msg
padding amount config = {config | padding = amount}

indent : Int  -> TreeConfig itemType msg -> TreeConfig itemType msg
indent amount config = {config | indent = amount}

rootItems : List itemType -> TreeConfig itemType msg -> TreeConfig itemType msg
rootItems items config = { config | rootItems = items} 

addRoot : itemType -> TreeConfig itemType msg -> TreeConfig itemType msg
addRoot item config = { config | rootItems = config.rootItems ++ [item]} 

children : (itemType -> List itemType ) -> TreeConfig itemType msg -> TreeConfig itemType msg
children f config = { config | itemChildren = f }

open : (itemType -> Bool ) -> TreeConfig itemType msg -> TreeConfig itemType msg
open f config = { config | itemOpen = f }

label : (itemType -> Html msg ) -> TreeConfig itemType msg -> TreeConfig itemType msg
label f config = { config | itemLabel = f }

icon : (itemType -> Maybe (Html msg) ) -> TreeConfig itemType msg -> TreeConfig itemType msg
icon f config = { config | itemIcon = f }

selected : (itemType -> Bool ) -> TreeConfig itemType msg -> TreeConfig itemType msg
selected f config = { config | itemSelected = f }

onPress : (itemType -> Maybe msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
onPress f config = {config | onPress = f}

onPressIcon : (itemType -> Maybe msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
onPressIcon f config = {config | onPressIcon = f}


treeNode : TreeConfig itemType msg -> Int -> itemType -> List (Html msg)
treeNode config totalIndent node =
  let 
    selectBg = case config.selectColor of
      Just c -> c
      Nothing -> config.baseColor
    nodeChildren = 
        if config.itemOpen node then 
          List.concatMap (treeNode config (totalIndent + config.indent)) (config.itemChildren node)
        else []
    selectedStyle = 
        if config.itemSelected node then 
          [ Attributes.class ("bg" ++ selectBg ++ " bgHover")
          , cssColor <| Colors.toCss <| C.rgb 1.0 1.0 1.0
          ]
        else 
          [ Attributes.class ("bg" ++ selectBg ++ " bgHoverOnly bgAlphaHigh text" ++ config.baseColor)
          ]
    borderStyle = if List.head config.rootItems == Just node then []
                  else [ cssBorderWidth4 (cssPx 1) (cssPx 0) (cssPx 0) (cssPx 0), cssBorderStyle "solid" ]

    iconButton = case config.itemIcon node of
                  Just icon_ -> 
                    let 
                      iconClickAttribute = 
                        case config.onPressIcon node of
                          Just msg -> [Events.onClick msg]
                          Nothing -> []
                    in
                    UI.Core.button iconClickAttribute icon_
                  Nothing -> none
    labelClickAttribute = Maybe.map (List.singleton << Events.onClick) (config.onPress node) |> Maybe.withDefault []
    labelButton = 
      UI.Core.button 
      ( [ Attributes.style "cursor" "pointer"
        , cssPadding4 (cssPx config.padding) "0" (cssPx config.padding) "0"
        , fullWidth
        ] ++ labelClickAttribute) (config.itemLabel node)
  in
  [ row 5
      ( [cssPadding4 "0" (cssPx config.padding) "0" (cssPx <| config.padding + totalIndent)
        , cssBorderColor <| Colors.toCss Colors.divider
        ] ++ selectedStyle ++ borderStyle )
      [iconButton, labelButton]
  ] ++ nodeChildren

tree : TreeConfig itemType msg -> Html msg
tree config =
  column 0 (textBody ++ [Attributes.style "align-items" "stretch"] ++ config.attributes)
    <| List.concatMap (treeNode config 0) config.rootItems
