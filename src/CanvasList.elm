module CanvasList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, buttonIdFor)

import Url
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import Update as U
import Config
import Utils exposing(iiifLink, pluralise, wrapKey, spinner)

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , sequence : Maybe SequenceUri
  , selectedCanvas : Maybe CanvasUri
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | SelectCanvas (Maybe CanvasUri)
          | CanvasClicked CanvasUri
          | IiifNotification Iiif.Notification

type OutMsg = CanvasOpened CanvasUri


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view }


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
      ({ model | manifest = Maybe.withDefault maybeManifestUri Nothing, selectedCanvas = Nothing }, Cmd.none, [])
    SelectCanvas canvasUri -> ({model | selectedCanvas = canvasUri}, Cmd.none, [])
    CanvasClicked canvasUri -> ({model | selectedCanvas = Just canvasUri}, Cmd.none, [CanvasOpened canvasUri])
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
            [spinner]
          else
            [ Keyed.node "div" [class "canvas_line"]
              (List.map (wrapKey (canvasButton model)) canvases )
              ]
--            (List.map (\c -> a [ href "#" ] [ canvasImgHtml c ]) canvases )
      Nothing -> []


buttonIdFor : CanvasUri -> String
buttonIdFor canvasUri = "canvas_button_" ++ (Config.shortenUri canvasUri)

canvasButton : Model -> Canvas -> Html Msg
canvasButton model canvas = 
  let
    width = round ((toFloat canvas.width) / (toFloat canvas.height) * 60.0)
    selected = if model.selectedCanvas == Just canvas.id then " selected" else ""
  in
  div [class ("canvas_button" ++ selected), id (buttonIdFor canvas.id)] 
    [ Button.button [ Button.roleLink, Button.attrs [style "width" ((String.fromInt width) ++ "px;"), class "canvas_preview", onClick (CanvasClicked canvas.id)]] [ canvasImgHtml canvas ]
    , div [class "canvas_label"] [text (Maybe.withDefault "" canvas.label)]
    ]

canvasImgHtml : Canvas -> Html msg
canvasImgHtml canvas = 
  img [ class "lazyload", src "spinner_40x60.gif", attribute "data-src" <| Iiif.canvasUrl (Iiif.FitH 60) canvas] []


