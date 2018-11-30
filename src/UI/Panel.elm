module UI.Panel exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Fonts exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

type SectionContent msg = PaddedContent (Html msg)
                        | DirectContent (Html msg)

type alias PanelConfig msg =
  { header: Maybe (Html msg)
  , sections: List (SectionContent msg)
  , footer: Maybe (Html msg)
  , round: Int
  , baseColor: String
  , endBackgroundColor: Maybe String
  , contentBackgroundColor: Maybe String
  , fullColor: Bool
  , attributes: List (Attribute msg)
  }

empty : PanelConfig msg
empty =
  { header = Nothing 
  , sections = []
  , footer = Nothing
  , round = 5
  , baseColor = "Primary"
  , endBackgroundColor = Nothing
  , contentBackgroundColor = Nothing
  , fullColor = False
  , attributes = []
  }


panel : PanelConfig msg -> Html msg
panel config = 
  column 0
    ( textBody ++ 
        [ cssBorderRadius <| cssPx config.round
        , Attributes.class ("border" ++ config.baseColor ++ " borderDarken")
        , cssBorderWidth <| cssPx 1
        , cssBorderStyle "solid"
        ]  ++ config.attributes )
    (  [ panelHeader config ] ++
      List.indexedMap (panelSection config) config.sections
    ++ [ panelFooter config ] )

panelEndAttributes : PanelConfig msg -> List (Attribute msg)
panelEndAttributes config = 
  let
    bgColor = case config.endBackgroundColor of
      Just c -> "bg" ++ c ++ " bgDesatHigh"
      Nothing -> "bg" ++ config.baseColor ++ " bgDesatHigh"
  in
    [ fullWidth
    , cssPadding4 (cssPx 10) (cssPx 15) (cssPx 10) (cssPx 15)
    , Attributes.class (bgColor ++ " text" ++ config.baseColor ++ " textDarken border" ++ config.baseColor ++ "borderDarken")
    ]

panelHeader : PanelConfig msg -> Html msg
panelHeader config =
  let 
    headerAttrs = 
      [ cssBorderWidth4 (cssPx 0) (cssPx 0) (cssPx 1) (cssPx 0)
      , cssBorderRadius4 (cssPx config.round) (cssPx config.round) (cssPx 0) (cssPx 0)
      , cssBorderStyle "solid"
      , Attributes.style "overflow" "hidden"
      ]
  in
  case config.header of
    Nothing -> none
    Just e ->
      el
        ( (panelEndAttributes config) ++ headerAttrs )
        e

panelFooter : PanelConfig msg -> Html msg
panelFooter config =
  let
    footerAttrs = 
      [ cssBorderWidth4 (cssPx 1) (cssPx 0) (cssPx 0) (cssPx 0)
      , cssBorderRadius4 (cssPx 0) (cssPx 0) (cssPx config.round) (cssPx config.round)
      , cssBorderStyle "solid"
      , Attributes.style "overflow" "hidden"
      ]
  in
  case config.footer of
    Nothing -> none
    Just e ->
      el
        ( (panelEndAttributes config) ++ footerAttrs )
        e

panelSection : PanelConfig msg -> Int -> SectionContent msg -> Html msg
panelSection config counter content_ =
  let
    borderStyle = 
      if counter > 0 then 
        [ cssBorderWidth4 (cssPx 1) (cssPx 0) (cssPx 0) (cssPx 0) 
        , cssBorderStyle "solid"
        ]
      else []
    bgStyle = case (config.contentBackgroundColor, config.fullColor) of
      (Just c, _) -> "bg" ++ c
      (Nothing, True) -> "bg" ++ config.baseColor ++ " bgDesatHigh"
      (Nothing, False) -> ""
    commonStyle = [ fullWidth
                  , Attributes.class <| bgStyle ++ " border" ++ config.baseColor ++ " borderDarken"
                  ] ++ borderStyle
  in
    case content_ of
      PaddedContent e -> el (commonStyle ++ [cssPadding <| cssPx 15, fullWidth]) e
      DirectContent e -> e


attributes : List (Attribute msg) -> PanelConfig msg -> PanelConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> PanelConfig msg -> PanelConfig msg
color color_ config = {config | baseColor = color_}

endBackgroundColor : String -> PanelConfig msg -> PanelConfig msg
endBackgroundColor color_ config = {config | endBackgroundColor = Just color_}

contentBackgroundColor : String -> PanelConfig msg -> PanelConfig msg
contentBackgroundColor color_ config = {config | contentBackgroundColor = Just color_}

fullColor : PanelConfig msg -> PanelConfig msg
fullColor config = {config | fullColor = True}

content : Html msg -> PanelConfig msg -> PanelConfig msg
content content_ config = {config | sections = [PaddedContent content_]}

addSection : Html msg -> PanelConfig msg -> PanelConfig msg
addSection content_ config = {config | sections = config.sections ++ [PaddedContent content_]}

addDirectSection : Html msg -> PanelConfig msg -> PanelConfig msg
addDirectSection content_ config = {config | sections = config.sections ++ [DirectContent content_]}

sections : List (SectionContent msg) -> PanelConfig msg -> PanelConfig msg
sections sections_ config = {config | sections = sections_}

round : Int -> PanelConfig msg -> PanelConfig msg
round amount config = {config | round = amount}

header : Html msg -> PanelConfig msg -> PanelConfig msg
header content_ config = {config | header = Just content_}

title : String -> PanelConfig msg -> PanelConfig msg
title text_ config = {config | header = Just (text text_)}

footer : Html msg -> PanelConfig msg -> PanelConfig msg
footer content_ config = {config | footer = Just content_}

popup : PanelConfig msg -> PanelConfig msg
popup = attributes [ Attributes.style "box-shadow" <| "0.0 0.0 3.0px 2.0px " ++ (Colors.toCss <| C.rgba 0.0 0.0 0.0 0.5) ]


default : PanelConfig msg
default = empty |> color "Secondary"

info : PanelConfig msg
info = empty |> color "Primary"

error : PanelConfig msg
error = empty |> color "Error"

warning : PanelConfig msg
warning = empty |> color "Warning"

success : PanelConfig msg
success = empty |> color "Success"

white : PanelConfig msg
white = 
  empty 
    |> color "Dim"
    |> endBackgroundColor "White"
