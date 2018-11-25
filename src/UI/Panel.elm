module UI.Panel exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Fonts exposing(..)

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input

type SectionContent msg = PaddedContent (Element msg)
                        | DirectContent (Element msg)

type alias PanelConfig msg =
  { header: Maybe (Element msg)
  , sections: List (SectionContent msg)
  , footer: Maybe (Element msg)
  , round: Int
  , width: Length
  , baseColor: Color
  , endBackgroundColor: Maybe Color
  , contentBackgroundColor: Maybe Color
  , fullColor: Bool
  , attributes: List (Attribute msg)
  }

empty : PanelConfig msg
empty =
  { header = Nothing 
  , sections = []
  , footer = Nothing
  , round = 5
  , width = fill
  , baseColor = Colors.lightBg
  , endBackgroundColor = Nothing
  , contentBackgroundColor = Nothing
  , fullColor = False
  , attributes = []
  }


panel : PanelConfig msg -> Element msg
panel config = 
  Element.column
    ( textBody ++ 
      [ Element.width config.width
      , Border.rounded config.round
      , Border.color <| C.darken 0.5 config.baseColor
      , Border.width 1
      , spacing 0
      ]
      ++ config.attributes )
      (  [ panelHeader config ] ++
        List.indexedMap (panelSection config) config.sections
      ++ [ panelFooter config ] )

panelEndAttributes : PanelConfig msg -> List (Attribute msg)
panelEndAttributes config = 
  let
    bgColor = case config.endBackgroundColor of
      Just c -> c
      Nothing -> C.desaturate 0.8 config.baseColor
  in
    [ Element.width fill
    , paddingEach {top = 10, bottom = 10, left = 15, right = 15}
    , Font.color <| C.darken 0.8 config.baseColor
    , Border.color <| C.darken 0.5 config.baseColor 
    , Background.color bgColor
    , spacing 5
    ]

panelHeader : PanelConfig msg -> Element msg
panelHeader config =
  let 
    headerAttrs = 
      [ Border.widthEach {bottom = 1, left = 0, right = 0, top = 0}
      , Border.roundEach {topLeft = config.round, topRight = config.round, bottomLeft = 0, bottomRight = 0}
      ]
  in
  case config.header of
    Nothing -> Element.none
    Just e ->
      Element.el
        ( (panelEndAttributes config) ++ headerAttrs )
        e

panelFooter : PanelConfig msg -> Element msg
panelFooter config =
  let
    footerAttrs = 
      [ Border.widthEach {bottom = 0, left = 0, right = 0, top = 1}
      , Border.roundEach {topLeft = 0, topRight = 0, bottomLeft = config.round, bottomRight = config.round}
      ]
  in
  case config.footer of
    Nothing -> Element.none
    Just e ->
      Element.el
        ( (panelEndAttributes config) ++ footerAttrs )
        e

panelSection : PanelConfig msg -> Int -> SectionContent msg -> Element msg
panelSection config counter content_ =
  let
    borderAttrs = 
      if counter > 0 then [ Border.widthEach {bottom = 0, left = 0, right = 0, top = 1} ]
      else []
    bgAttrs = case (config.contentBackgroundColor, config.fullColor) of
      (Just c, _) -> [Background.color c]
      (Nothing, True) -> [Background.color (C.desaturate 0.8 config.baseColor)]
      (Nothing, False) -> []
    commonAttrs = [ Element.width fill
                  , Border.color <| C.darken 0.5 config.baseColor 
                  ] ++ borderAttrs ++ bgAttrs
  in
    case content_ of
      PaddedContent e -> Element.el (commonAttrs ++ [padding 15]) e
      DirectContent e -> e


attributes : List (Attribute msg) -> PanelConfig msg -> PanelConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : Color -> PanelConfig msg -> PanelConfig msg
color color_ config = {config | baseColor = color_}

endBackgroundColor : Color -> PanelConfig msg -> PanelConfig msg
endBackgroundColor color_ config = {config | endBackgroundColor = Just color_}

contentBackgroundColor : Color -> PanelConfig msg -> PanelConfig msg
contentBackgroundColor color_ config = {config | contentBackgroundColor = Just color_}

fullColor : PanelConfig msg -> PanelConfig msg
fullColor config = {config | fullColor = True}

content : Element msg -> PanelConfig msg -> PanelConfig msg
content content_ config = {config | sections = [PaddedContent content_]}

addSection : Element msg -> PanelConfig msg -> PanelConfig msg
addSection content_ config = {config | sections = config.sections ++ [PaddedContent content_]}

addDirectSection : Element msg -> PanelConfig msg -> PanelConfig msg
addDirectSection content_ config = {config | sections = config.sections ++ [DirectContent content_]}

sections : List (SectionContent msg) -> PanelConfig msg -> PanelConfig msg
sections sections_ config = {config | sections = sections_}

round : Int -> PanelConfig msg -> PanelConfig msg
round amount config = {config | round = amount}

header : Element msg -> PanelConfig msg -> PanelConfig msg
header content_ config = {config | header = Just content_}

title : String -> PanelConfig msg -> PanelConfig msg
title text_ config = {config | header = Just (text text_)}

footer : Element msg -> PanelConfig msg -> PanelConfig msg
footer content_ config = {config | footer = Just content_}

width : Length -> PanelConfig msg -> PanelConfig msg
width width_ config = {config | width = width_}

popup : PanelConfig msg -> PanelConfig msg
popup = attributes [ Border.shadow { offset=(0.0, 0.0), size=1.0, blur=3.0, color=rgba 0.0 0.0 0.0 0.5} ]


default : PanelConfig msg
default = empty |> color Colors.secondary

info : PanelConfig msg
info = empty |> color Colors.primary

error : PanelConfig msg
error = empty |> color Colors.error

warning : PanelConfig msg
warning = empty |> color Colors.warning

success : PanelConfig msg
success = empty |> color Colors.success

white : PanelConfig msg
white = 
  empty 
    |> color (rgb 0.5 0.5 0.5)
    |> endBackgroundColor (rgb 1.0 1.0 1.0) 
