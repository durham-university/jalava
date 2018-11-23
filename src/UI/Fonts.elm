module UI.Fonts exposing(..)

import Element exposing(..)
import Element.Font as Font

import UI.Colors as Colors

textBody : List (Attribute msg)
textBody = 
  [ Font.size 16
  , Font.color Colors.defaultTextColor
  ]
