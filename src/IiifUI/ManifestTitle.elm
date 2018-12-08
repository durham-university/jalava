module IiifUI.ManifestTitle exposing(..)

import Iiif.Types exposing(..)
import Iiif.Utils
import Iiif.ImageApi as ImageApi

import UI.Core exposing(..)
import UI.TitleLine as TitleLine
import UI.Fonts exposing(..)
import IiifUI.Spinner as Spinner

import Html exposing (..)
import Html.Attributes as Attributes 
import Html.Events as Events


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

attributes : List (Attribute msg)  -> ManifestTitleConfig msg -> ManifestTitleConfig msg
attributes attrs config = {config | attributes = attrs}

manifestTitle : ManifestTitleConfig msg -> Html msg
manifestTitle config =
  case config.manifest of
    Nothing -> none
    Just manifest_ ->
      let
        logoUrlMaybe = Maybe.map (ImageApi.resourceUrlSimple (ImageApi.FitH config.logoHeight)) manifest_.logo
        logo = Maybe.map (\justLogo -> img [Attributes.height config.logoHeight, Attributes.src justLogo, Attributes.alt "logo"] []) logoUrlMaybe
        title = manifest_.label |> Maybe.map ( p [fullWidth, Attributes.style "flex-shrink" "1"] << List.singleton << text) |> Maybe.withDefault none 
        spinner = if Iiif.Utils.isStub manifest_ then Spinner.spinner
                  else none
      in
        TitleLine.empty
          |> TitleLine.attributes (textBody ++ [Attributes.style "min-height" <| cssPx config.logoHeight] ++ config.attributes)
          |> TitleLine.maybeIcon logo
          |> TitleLine.content (row 5 [fullWidth] [title, spinner])
          |> TitleLine.titleLine

simple : Manifest -> Html msg
simple manifest_ =
  empty |> manifest manifest_ |> manifestTitle