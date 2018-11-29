module IiifUI.Spinner exposing(..)

import UI.Core exposing(..)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

spinner : Html msg
spinner = img [Attributes.src "spinner.gif", Attributes.alt "Loading"] []

spinnerThumbnail : Html msg
spinnerThumbnail = img [Attributes.src "spinner_40x60.gif", Attributes.alt "Loading"] []