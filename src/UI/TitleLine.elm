module UI.TitleLine exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Icon as Icon
import UI.Fonts exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


type alias TitleLineConfig msg =
  { content: Maybe (Html msg)
  , icon: Maybe (Html msg)
  , attributes: List (Attribute msg)
  }

empty : TitleLineConfig msg
empty =
  { content = Nothing
  , icon = Nothing
  , attributes = []
  }

titleLine : TitleLineConfig msg -> Html msg
titleLine config = 
  row 5
    config.attributes
    <| List.filterMap identity 
      [ config.icon
      , Maybe.map (el [Attributes.style "flex-shrink" "1"]) config.content
      ]

attributes : List (Attribute msg) -> TitleLineConfig msg -> TitleLineConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}


content : Html msg -> TitleLineConfig msg -> TitleLineConfig msg
content content_ config = {config | content = Just content_}

maybeContent : Maybe (Html msg) -> TitleLineConfig msg -> TitleLineConfig msg
maybeContent content_ config = {config | content = content_}

icon : Html msg -> TitleLineConfig msg -> TitleLineConfig msg
icon icon_ config = {config | icon = Just icon_}

maybeIcon : Maybe (Html msg) -> TitleLineConfig msg -> TitleLineConfig msg
maybeIcon icon_ config = {config | icon = icon_}


simple : String -> Html msg
simple content_ = empty |> content (text content_) |> titleLine

withIcon : String -> String -> Html msg
withIcon icon_ content_ = empty |> icon (Icon.icon icon_ []) |> content (text content_) |> titleLine

iconOnly : String -> Html msg
iconOnly icon_ = empty |> icon (Icon.icon icon_ []) |> titleLine