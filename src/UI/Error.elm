module UI.Error exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Core exposing(..)
import UI.Icon as Icon
import UI.Fonts exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes


type alias ErrorConfig msg =
  { content: Html msg
  , baseColor: String
  , attributes: List (Attribute msg)
  }

emptyConfig : ErrorConfig msg
emptyConfig =
  { content = none
  , baseColor = "Error"
  , attributes = []
  }

error : ErrorConfig msg -> Html msg
error config = 
  row 5
    ( textBody ++ 
      [ cssPadding <| cssPx 5
      , cssBorderRadius <| cssPx 5
      , Attributes.class ("bg" ++ config.baseColor ++ " bgDesat border" ++ config.baseColor ++ " text" ++ config.baseColor ++ " textDarken")
      , cssBorderWidth <| cssPx 1
      , cssBorderStyle "solid"
      ]
      ++ config.attributes )
    [ Icon.icon "exclamation-triangle" []
    , config.content
    ]

attributes : List (Attribute msg) -> ErrorConfig msg -> ErrorConfig msg
attributes attrs config = {config | attributes = config.attributes ++ attrs}

color : String -> ErrorConfig msg -> ErrorConfig msg
color color_ config = {config | baseColor = color_}

content : Html msg -> ErrorConfig msg -> ErrorConfig msg
content content_ config = {config | content = content_}

warning : ErrorConfig msg
warning = emptyConfig |> color "Warning"

err : String -> Html msg
err s = emptyConfig |> content (text s) |> error