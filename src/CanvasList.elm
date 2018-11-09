module CanvasList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

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

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , sequence : Maybe SequenceUri
  , selectedCanvas : Maybe CanvasUri
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | IiifNotification Iiif.Notification

type OutMsg = CanvasSelected CanvasUri


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

emptyModel : Model
emptyModel = 
  { iiif = Iiif.empty
  , manifest = Nothing
  , sequence = Nothing
  , selectedCanvas = Nothing
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> 
      ({ model | selectedCanvas = Maybe.withDefault maybeManifestUri Nothing }, Cmd.none, [])
    IiifNotification notification -> (model, Cmd.none, [])

view : Model -> Html Msg
view model = 
  div [ class "canvas_list" ] <|
    case model.manifest of
      Just manifestUri ->
        let
          manifest = getManifest model.iiif manifestUri
          maybeSequence = List.head manifest.sequences
          maybeCanvases = Maybe.map .canvases maybeSequence
          canvases = Maybe.withDefault [] maybeCanvases
        in
          if isStub manifest then
            [i [ class "spinner fas fa-spinner" ] []]
          else
            (List.map (\c -> a [ href "#" ] [ canvasImgHtml c ]) canvases )
      Nothing -> []

canvasImgHtml : Canvas -> Html msg
canvasImgHtml canvas = 
  img [ class "lazyload canvas_preview", src "spinner_40x60.gif", attribute "data-src" <| Iiif.canvasUrl (Iiif.FitH 60) canvas] []


