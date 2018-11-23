module UI.TitleLine exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Icon as Icon
import UI.Fonts exposing(..)

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


type alias TitleLineConfig msg =
  { content: Element msg
  , icon: Maybe (Element msg)
  , attributes: List (Attribute msg)
  }

empty : TitleLineConfig msg
empty =
  { content = Element.none
  , icon = Nothing
  , attributes = []
  }

titleLine : TitleLineConfig msg -> Element msg
titleLine config = 
  Element.row
    ([ spacing 5 ] ++ config.attributes)
    <| List.filterMap identity 
      [ config.icon
      , Just config.content
      ]

attributes : List (Attribute msg) -> TitleLineConfig msg -> TitleLineConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

content : Element msg -> TitleLineConfig msg -> TitleLineConfig msg
content content_ config = {config | content = content_}

icon : Element msg -> TitleLineConfig msg -> TitleLineConfig msg
icon icon_ config = {config | icon = Just icon_}

maybeIcon : Maybe (Element msg) -> TitleLineConfig msg -> TitleLineConfig msg
maybeIcon icon_ config = {config | icon = icon_}


simple : String -> Element msg
simple content_ = empty |> content (text content_) |> titleLine

withIcon : String -> String -> Element msg
withIcon icon_ content_ = empty |> icon (Icon.icon icon_ []) |> content (text content_) |> titleLine

iconOnly : String -> Element msg
iconOnly icon_ = empty |> icon (Icon.icon icon_ []) |> titleLine