port module StructuresView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Element

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
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvas : Maybe CanvasUri
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
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
  { iiif = Iiif.Utils.empty
  , manifest = Nothing
  , canvas = Nothing
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> ({model | manifest = maybeManifestUri, canvas = Nothing}, Cmd.none, [])
    SetCanvas maybeCanvasUri -> ({model | canvas = maybeCanvasUri}, Cmd.none, [])
    RangeClicked rangeUri -> (model, Cmd.none, [RangeSelected rangeUri])
    IiifNotification notification -> (model, Cmd.none, [])

view : Model -> Element.Element Msg
view model = 
  case model.manifest of
    Nothing -> Element.none
    Just manifestUri ->
      let manifest = getManifest model.iiif manifestUri
      in
      Tree.empty
        |> Tree.rootItems (getTopRanges manifest)
        |> Tree.label (Element.text << Maybe.withDefault "unnamed" << .label)
        |> Tree.children (getRanges manifest << .ranges)
        |> Tree.selected (List.any (\c -> model.canvas == Just c) << .canvases)
        |> Tree.onPress (Just << RangeClicked << .id)
        |> Tree.tree

{-
view : Model -> Html Msg
view model = 
  let
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    topRanges = Maybe.withDefault [] <| Maybe.map getTopRanges maybeManifest
    rangesHtml = case maybeManifest of
                  Just manifest -> List.map (rangeHtml model manifest) topRanges
                  Nothing -> []
  in
  Grid.row [ Row.attrs [class "structures_view"] ] [Grid.col [] rangesHtml ]

rangeHtml : Model -> Manifest -> Range -> Html Msg
rangeHtml model manifest range = 
  let
    subRanges = getRanges manifest range.ranges
    label = Maybe.withDefault "unnamed" range.label
    selected = List.any (\r -> model.canvas == Just r) range.canvases
    selectedClass = if selected then " selected" else ""
  in
  div [ class ("range_node" ++ selectedClass) ] 
    [ span [ class "title" ] [ Button.button [ Button.roleLink, Button.attrs [ onClick (RangeClicked range.id)]] [text label] ]
    , div [class "sub_ranges"] (List.map (rangeHtml model manifest) subRanges)
    ]

-}