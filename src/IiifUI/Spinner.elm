module IiifUI.Spinner exposing(..)

import UI.Core exposing(..)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

spinner : Html msg
spinner = img [Attributes.src "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==", Attributes.class "spinner", Attributes.alt "Loading"] []

spinnerThumbnail : Html msg
spinnerThumbnail = img [Attributes.src "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==", Attributes.class "spinner_40x60", Attributes.alt "Loading"] []