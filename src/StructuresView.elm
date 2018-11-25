port module StructuresView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Element
import Element.Lazy as Lazy

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import Update as U
import Utils exposing(pluralise)

import ManifestDetails exposing(..)

import Iiif.Types exposing(..)
import Iiif.Loading
import Iiif.Utils exposing(..)

import UI.Tree as Tree
import UI.Icon as Icon

import Element

type alias Model =
  { manifest : Maybe Manifest
  , canvas : Maybe CanvasUri
  }

type Msg  = SetManifest (Maybe Manifest)
          | SetCanvas (Maybe CanvasUri)
          | RangeClicked RangeUri
          | IiifNotification Iiif.Loading.Notification

type OutMsg = RangeSelected RangeUri


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])


emptyModel : Model
emptyModel  = 
  { manifest = Nothing
  , canvas = Nothing
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifest -> ({model | manifest = maybeManifest, canvas = Nothing}, Cmd.none, [])
    SetCanvas maybeCanvasUri -> ({model | canvas = maybeCanvasUri}, Cmd.none, [])
    RangeClicked rangeUri -> (model, Cmd.none, [RangeSelected rangeUri])
    IiifNotification notification ->
      case notification of 
        Iiif.Loading.ManifestLoaded iiif manifestUri -> 
          if Just manifestUri == Maybe.map .id model.manifest then
            ({model | manifest = Just <| getManifest iiif manifestUri}, Cmd.none, [])
          else (model, Cmd.none, [])
        _ -> (model, Cmd.none, [])

view : Model -> Element.Element Msg
view model = Lazy.lazy view_ model

view_ : Model -> Element.Element Msg
view_ model = 
  case model.manifest of
    Nothing -> Element.none
    Just manifest ->
      Tree.empty
        |> Tree.rootItems (getTopRanges manifest)
        |> Tree.label (Element.text << Maybe.withDefault "unnamed" << .label)
        |> Tree.children (getRanges manifest << .ranges)
        |> Tree.selected (List.any (\c -> model.canvas == Just c) << .canvases)
        |> Tree.onPress (Just << RangeClicked << .id)
        |> Tree.tree
