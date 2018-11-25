port module CanvasList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, setContainerId)

import Url
import Json.Decode as Decode
import Json.Encode as Encode

import Html.Attributes
import Element exposing (..)

import IiifUI.Spinner as Spinner
import IiifUI.CanvasButton as CanvasButton

import Update as U
import Utils exposing(pluralise, wrapKey, sanitiseId)

import Iiif.Types exposing(..)
import Iiif.ImageApi
import Iiif.Loading
import Iiif.Utils exposing(getManifest, isStub)
import UriMapper exposing (UriMapper)

type alias Model =
  { iiif : Iiif
  , containerId : Maybe String
  , manifest : Maybe ManifestUri
  , sequence : Maybe SequenceUri
  , selectedCanvas : Maybe CanvasUri
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | SelectCanvas (Maybe CanvasUri)
          | CanvasClicked CanvasUri
          | ScrollToView CanvasUri Bool
          | IiifNotification Iiif.Loading.Notification

type OutMsg = CanvasOpened CanvasUri

port outPortScrollToView : Encode.Value -> Cmd msg


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

setContainerId : String -> Model -> Model
setContainerId id_ model = {model | containerId = Just id_}

emptyModel : Model
emptyModel = 
  { iiif = Iiif.Utils.empty
  , containerId = Nothing
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
    ScrollToView canvasUri animate -> scrollToView canvasUri animate model

view : Model -> Element Msg
view model = 
    case model.manifest of
      Just manifestUri ->
        let
          manifest = getManifest model.iiif manifestUri
          maybeSequence = List.head manifest.sequences
          maybeCanvases = Maybe.map .canvases maybeSequence
          canvases = Maybe.withDefault [] maybeCanvases
          canvasButton canvas = 
            CanvasButton.empty 
              |> CanvasButton.canvas canvas 
              |> CanvasButton.includeLabel 
              |> CanvasButton.onPress (CanvasClicked canvas.id)
              |> CanvasButton.attributes [htmlAttribute <| Html.Attributes.id (buttonIdFor model canvas.id)]
              |> CanvasButton.selected (Just canvas.id == model.selectedCanvas)
              |> CanvasButton.canvasButton
          idAttribute = case model.containerId of
            Just containerId -> [htmlAttribute <| Html.Attributes.id containerId]
            Nothing -> []
        in
          if isStub manifest then row [padding 10, spacing 10, width fill, scrollbarX] [Spinner.spinnerThumbnail]
          else
            row ([padding 10, spacing 10, width fill, scrollbarX] ++ idAttribute) (List.map canvasButton canvases)
      Nothing -> Element.none


scrollToView : CanvasUri -> Bool -> Model -> (Model, Cmd Msg, List OutMsg)
scrollToView canvasUri animate model =
  case (Debug.log "scrollToView" model.containerId) of
    Nothing -> (model, Cmd.none, [])
    Just containerId -> 
      let 
        buttonId = buttonIdFor model canvasUri
        scrollCmd = outPortScrollToView (Encode.object
          [ ("container", Encode.string <| "#" ++ containerId)
          , ("item", Encode.string <| "#" ++ buttonId)
          , ("axis", Encode.string "x")
          , ("animate", Encode.bool animate)
          ])
      in (model, scrollCmd, [])


buttonIdFor : Model -> CanvasUri -> String
buttonIdFor model canvasUri = 
  (Maybe.withDefault "" model.containerId) ++ "_canvas_button_" ++ (sanitiseId canvasUri)
