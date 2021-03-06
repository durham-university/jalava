module UI.Colors exposing(..)

import UI.ColorUtils exposing (..)

import Html.Attributes exposing (..)

type alias Color = UI.ColorUtils.Color

toCss : Color -> String
toCss (RGBA r g b a) = 
  "rgba(" ++ 
  (String.fromInt <| round <| 255.0 * r) ++ "," ++ 
  (String.fromInt <| round <| 255.0 * g) ++ "," ++ 
  (String.fromInt <| round <| 255.0 * b) ++ "," ++
  (String.fromFloat a) ++ ")"

primary : Color
primary = normalize 1.0 (rgb 0.0 0.5 1.0)

secondary : Color
secondary = normalize 1.0 (rgb 1.0 1.0 1.0)

success : Color
success = normalize 1.0 (rgb 0.3 0.7 0.2)

error : Color
error = normalize 1.0 (rgb 0.7 0.3 0.2)

warning : Color
warning = normalize 1.0 (rgb 1.0 0.9 0.1)

link : Color
link = primary

defaultBackground : Color
defaultBackground = rgb 1.0 1.0 1.0

lightBg : Color
lightBg = rgb 0.9 0.9 0.9 

divider : Color
divider = rgb 0.8 0.8 0.8

defaultTextColor : Color
defaultTextColor = rgb 0.2 0.2 0.2

dimTextColor : Color
dimTextColor = rgb 0.4 0.4 0.4
