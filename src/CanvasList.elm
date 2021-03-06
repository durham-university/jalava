module CanvasList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, setContainerId)

import Url
import Json.Decode as Decode
import Json.Encode as Encode

import UI.Core exposing(..)
import UI.Error exposing(err)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy exposing(lazy)

import IiifUI.Spinner as Spinner
import IiifUI.CanvasButton as CanvasButton

import Update as U
import Utils exposing(pluralise, wrapKey, sanitiseId, ScrollInfo, ScrollAlignment(..), ScrollAxis(..), ScrollTarget(..))

import Iiif.Types exposing(..)
import Iiif.ImageApi
import Iiif.Loading
import Iiif.Utils exposing(getManifest, willLoad)
import UriMapper exposing (UriMapper)

type alias Model =
  { containerId : Maybe String
  , manifest : Maybe Manifest
  , sequence : Maybe SequenceUri
  , selectedCanvas : Maybe CanvasUri
  }

type Msg  = SetManifest (Maybe Manifest)
          | SelectCanvas (Maybe CanvasUri)
          | CanvasClicked CanvasUri
          | ScrollToView CanvasUri Bool
          | IiifNotification Iiif.Loading.Notification

type OutMsg = CanvasOpened CanvasUri
            | ScrollToViewOut ScrollInfo

component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

setContainerId : String -> Model -> Model
setContainerId id_ model = {model | containerId = Just id_}

emptyModel : Model
emptyModel = 
  { containerId = Nothing
  , manifest = Nothing
  , sequence = Nothing
  , selectedCanvas = Nothing
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifest -> 
      ({ model | manifest = maybeManifest, selectedCanvas = Nothing }, Cmd.none, [])
    SelectCanvas canvasUri -> ({model | selectedCanvas = canvasUri}, Cmd.none, [])
    CanvasClicked canvasUri -> ({model | selectedCanvas = Just canvasUri}, Cmd.none, [CanvasOpened canvasUri])
    IiifNotification notification -> 
      case notification of 
        Iiif.Loading.ManifestLoaded iiif manifestUri -> 
          if Just manifestUri == Maybe.map .id model.manifest then
            ({model | manifest = Just <| getManifest iiif manifestUri}, Cmd.none, [])
          else (model, Cmd.none, [])
        _ -> (model, Cmd.none, [])
    ScrollToView canvasUri animate -> scrollToView canvasUri animate model

view : Model -> Html Msg
view model = lazy view_ model

view_ : Model -> Html Msg
view_ model = 
  let
    idAttribute = case model.containerId of
      Just containerId -> [Attributes.id containerId]
      Nothing -> []
  in
    case model.manifest of
      Just manifest ->
        let
          maybeSequence = List.head manifest.sequences
          maybeCanvases = Maybe.map .canvases maybeSequence
          canvases = Maybe.withDefault [] maybeCanvases
          canvasButton canvas = 
            CanvasButton.empty 
              |> CanvasButton.canvas canvas 
              |> CanvasButton.includeLabel 
              |> CanvasButton.onPress (CanvasClicked canvas.id)
              |> CanvasButton.attributes [Attributes.id (buttonIdFor model canvas.id)]
              |> CanvasButton.selected (Just canvas.id == model.selectedCanvas)
              |> CanvasButton.canvasButton
        in
          if willLoad manifest then row 10 [cssPadding <| cssPx 10, fullWidth, Attributes.style "overflow-x" "scroll"] [Spinner.spinnerThumbnail]
          else
            row 10 ([cssPadding <| cssPx 10, fullWidth, Attributes.style "overflow-x" "scroll"] ++ idAttribute) (List.map canvasButton canvases)
      Nothing -> row 10 ([cssPadding <| cssPx 10, fullWidth, Attributes.style "overflow-x" "scroll"] ++ idAttribute) []


scrollToView : CanvasUri -> Bool -> Model -> (Model, Cmd Msg, List OutMsg)
scrollToView canvasUri animate model =
  case model.containerId of
    Nothing -> (model, Cmd.none, [])
    Just containerId -> 
      let 
        buttonId = buttonIdFor model canvasUri
      in (model, Cmd.none, [ScrollToViewOut <| ScrollInfo containerId (ScrollRef buttonId) ScrollX animate ScrollMiddle])


buttonIdFor : Model -> CanvasUri -> String
buttonIdFor model canvasUri = 
  (Maybe.withDefault "" model.containerId) ++ "_canvas_button_" ++ (sanitiseId canvasUri)
