module UI.Tree exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Fonts exposing(..)
import UI.Icon exposing(icon)
import UI.Button as Button

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events

type alias TreeConfig itemType msg =
  { rootItems : List itemType
  , itemChildren : itemType -> List itemType
  , itemOpen : itemType -> Bool
  , itemLabel : itemType -> Element msg
  , itemIcon : itemType -> Maybe (Element msg)
  , itemSelected : itemType -> Bool
  , onPress : itemType -> Maybe msg
  , onPressIcon : itemType -> Maybe msg
  , baseColor : Color
  , selectColor : Maybe Color
  , padding : Int
  , indent : Int
  , attributes : List (Attribute msg)
  }

empty : TreeConfig itemType msg
empty =
  { rootItems = []
  , itemChildren = \_ -> []
  , itemOpen = \_ -> True
  , itemLabel = \_ -> Element.none
  , itemIcon = \_ -> Nothing
  , itemSelected = \_ -> False
  , onPress = \_ -> Nothing
  , onPressIcon = \_ -> Nothing
  , baseColor = Colors.primary
  , selectColor = Nothing
  , padding = 10
  , indent = 20
  , attributes = []
  }

attributes : List (Attribute msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : Color -> TreeConfig itemType msg -> TreeConfig itemType msg
color color_ config = {config | baseColor = color_}

selectColor : Color -> TreeConfig itemType msg -> TreeConfig itemType msg
selectColor color_ config = {config | selectColor = Just color_}

maybeSelectColor : Maybe Color -> TreeConfig itemType msg -> TreeConfig itemType msg
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

label : (itemType -> Element msg ) -> TreeConfig itemType msg -> TreeConfig itemType msg
label f config = { config | itemLabel = f }

icon : (itemType -> Maybe (Element msg) ) -> TreeConfig itemType msg -> TreeConfig itemType msg
icon f config = { config | itemIcon = f }

selected : (itemType -> Bool ) -> TreeConfig itemType msg -> TreeConfig itemType msg
selected f config = { config | itemSelected = f }

onPress : (itemType -> Maybe msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
onPress f config = {config | onPress = f}

onPressIcon : (itemType -> Maybe msg) -> TreeConfig itemType msg -> TreeConfig itemType msg
onPressIcon f config = {config | onPressIcon = f}


treeNode : TreeConfig itemType msg -> Int -> itemType -> List (Element msg)
treeNode config totalIndent node =
  let 
    selectBg = case config.selectColor of
      Just c -> c
      Nothing -> config.baseColor
    nodeChildren = 
        if config.itemOpen node then 
          List.concatMap (treeNode config (totalIndent + config.indent)) (config.itemChildren node)
        else []
    selectedAttrs = 
        if config.itemSelected node then 
          [ selectBg |> Background.color
          , rgb 1.0 1.0 1.0 |> Font.color
          , mouseOver [ selectBg |> C.darken 0.1 |> Background.color ]
          ]
        else 
          [ mouseOver [ selectBg |> C.desaturate 0.9 |> Background.color ]
          ]
    borderAttrs = if List.head config.rootItems == Just node then []
                  else [ Border.widthEach {bottom = 0, left = 0, right = 0, top = 1} ]

    iconButton = case config.itemIcon node of
                  Just icon_ -> Input.button [] { onPress = config.onPressIcon node, label = icon_ }
                  Nothing -> Element.none
    labelButton = Input.button [] {onPress = config.onPress node, label = config.itemLabel node}
  in
  [ row
      ( [ paddingEach {left = config.padding + totalIndent, top = config.padding, bottom = config.padding, right = config.padding}
        , width fill
        , Border.color Colors.divider
        , focused [Border.shadow { offset=(0.0, 0.0), size=0.0, blur=0.0, color = rgb 0.0 0.0 0.0}]
        , spacing 5
        ] ++ selectedAttrs ++ borderAttrs)
      [iconButton, labelButton]
  ] ++ nodeChildren

tree : TreeConfig itemType msg -> Element msg
tree config =
  Element.column (textBody ++ [Font.color config.baseColor] ++ config.attributes)
    <| List.concatMap (treeNode config 0) config.rootItems
