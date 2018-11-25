module IiifUI.ManifestTitle exposing(..)

import Iiif.Types exposing(..)
import Iiif.Utils

import Element exposing(..)
import UI.TitleLine as TitleLine
import IiifUI.Spinner as Spinner

type alias ManifestTitleConfig msg =
  { manifest : Maybe Manifest
  , logoHeight : Int
  , attributes : List (Attribute msg) 
  }

empty : ManifestTitleConfig msg
empty =
  { manifest = Nothing
  , logoHeight = 24
  , attributes = []
  }

manifest : Manifest -> ManifestTitleConfig msg -> ManifestTitleConfig msg
manifest manifest_ config = {config | manifest = Just manifest_} 

logoHeight : Int -> ManifestTitleConfig msg -> ManifestTitleConfig msg
logoHeight i config = {config | logoHeight = i}

attributes : List (Element.Attribute msg)  -> ManifestTitleConfig msg -> ManifestTitleConfig msg
attributes attrs config = {config | attributes = attrs}

manifestTitle : ManifestTitleConfig msg -> Element msg
manifestTitle config =
  case config.manifest of
    Nothing -> Element.none
    Just manifest_ ->
      let
        logo = Maybe.map (\justLogo -> Element.image [height <| px config.logoHeight] {src = justLogo, description = "logo"}) manifest_.logo
        title = manifest_.label |> Maybe.map (Element.paragraph [width fill] << List.singleton << Element.text) |> Maybe.withDefault Element.none 
        spinner = if Iiif.Utils.isStub manifest_ then Spinner.spinner
                  else Element.none
      in
        TitleLine.empty
          |> TitleLine.attributes ([height <| px config.logoHeight] ++ config.attributes)
          |> TitleLine.maybeIcon logo
          |> TitleLine.content (row [spacing 5, width fill] [title, spinner])
          |> TitleLine.titleLine

simple : Manifest -> Element msg
simple manifest_ =
  empty |> manifest manifest_ |> manifestTitle