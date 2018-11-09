module ManifestView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Url
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import Update as U
import Config
import Utils exposing(iiifLink, pluralise)

import CanvasList

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvasListModel : CanvasList.Model
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | CanvasListMsg CanvasList.Msg
          | IiifNotification Iiif.Notification

type OutMsg = CanvasSelected CanvasUri


canvasListSubModel : Model -> CanvasList.Model
canvasListSubModel model =
  let subModel = model.canvasListModel
  in { subModel | iiif = model.iiif }

canvasListOutMapper : CanvasList.OutMsg -> OutMsg
canvasListOutMapper msg =
  case msg of
    CanvasList.CanvasSelected uri -> CanvasSelected uri

canvasListUpdater : CanvasList.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
canvasListUpdater msg model =
  CanvasList.update msg (canvasListSubModel model)
    |> canvasListPipe model

canvasListPipe : Model -> (CanvasList.Model, Cmd CanvasList.Msg, List CanvasList.OutMsg) -> (Model, Cmd Msg, List OutMsg)
canvasListPipe model = 
  U.mapCmd CanvasListMsg
  >> U.mapModel (\m -> { model | canvasListModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut canvasListOutMapper



init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    CanvasList.init flags
      |> canvasListPipe baseModel
--      |> U.chain (openUrl url)


emptyModel : Model
emptyModel  = 
  { iiif = Iiif.empty
  , manifest = Nothing
  , canvasListModel = CanvasList.emptyModel
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> 
      ({model | manifest = maybeManifestUri}, Cmd.none, [])
        |> U.chain (canvasListUpdater (CanvasList.SetManifest maybeManifestUri))
    CanvasListMsg canvasListMsg -> canvasListUpdater canvasListMsg model
    IiifNotification notification -> 
      (model, Cmd.none, [])
        |> U.chain (canvasListUpdater (CanvasList.IiifNotification notification))

view : Model -> Html Msg
view model = 
  let
    canvasList = Html.map CanvasListMsg <| CanvasList.view (canvasListSubModel model)
  in
  div [ class "manifest_view" ] [
    div [ class "manifest_zoomer" ] [],
    canvasList
  ]
