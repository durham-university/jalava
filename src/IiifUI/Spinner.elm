module IiifUI.Spinner exposing(..)

import Element exposing(..)

spinner : Element msg
spinner = image [] {src = "spinner.gif", description = "Loading"}

spinnerThumbnail : Element msg
spinnerThumbnail = image [] {src = "spinner_40x60.gif", description = "Loading"}