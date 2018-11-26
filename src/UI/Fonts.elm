module UI.Fonts exposing(..)

import Html exposing(Attribute)
import Html.Attributes as Attributes

import UI.Colors as Colors

textBody : List (Attribute msg)
textBody = 
  [ Attributes.style "font-size" "16px"
  , Attributes.style "font-family" "sans-serif"
  ]
