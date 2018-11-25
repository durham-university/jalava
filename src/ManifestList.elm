module ManifestList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, subscriptions)

import Json.Decode as Decode
import Regex
import List.Extra as ListE

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getCollection, isStub, manifestToString)
import Iiif.Loading
import Iiif.ImageApi

import Element exposing(..)
import Element.Keyed as Keyed

import IiifUI.ManifestPanel as ManifestPanel

import ManifestDetails
import Utils exposing(pluralise, wrapKey)
import Update as U


type alias Model = 
  { iiif : Iiif
  , manifests : List ManifestUri
  , collection : Maybe CollectionUri
  , allManifests : List ManifestUri
  , selectedManifest : Maybe ManifestUri
  , panelModels : List (ManifestPanel.Model)
  , errors : List String
  }

type Msg = AddManifest ManifestUri
         | ClearManifests
         | SetManifests (List ManifestUri)
         | SetCollection CollectionUri
         | ClearCollection
         | ManifestClicked ManifestUri
         | CanvasClicked ManifestUri CanvasUri
         | IiifNotification Iiif.Loading.Notification
         | ManifestPanelMsg Int ManifestPanel.Msg

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | ManifestSelected ManifestUri
            | CanvasSelected ManifestUri CanvasUri


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }

manifestPanel ind = 
  U.subComponent 
    { component = ManifestPanel.component 
    , unwrapModel = \model -> 
        case ListE.getAt ind model.panelModels of
          Just panelModel -> panelModel
          Nothing -> Debug.log "Panel model not found" ManifestPanel.emptyModel
    , wrapModel = \model subModel -> { model | panelModels = ListE.setAt ind subModel model.panelModels }
    , wrapMsg = ManifestPanelMsg ind
    , outEvaluator = \msgSub model ->
        case (msgSub, ListE.getAt ind model.allManifests) of
          (_, Nothing) -> (model, Cmd.none, [])
          (ManifestPanel.ManifestSelected, Just manifestUri) -> (model, Cmd.none, [ManifestSelected manifestUri])
          (ManifestPanel.CanvasSelected canvasUri, Just manifestUri) -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }

updateAllPanels : ManifestPanel.Msg -> Model -> (Model, Cmd Msg, List OutMsg )
updateAllPanels msg model =
  let
    folder : a -> (Int, (Model, Cmd Msg, List OutMsg)) -> (Int, (Model, Cmd Msg, List OutMsg))
    folder _ (index, result) =
      (index + 1, result |> U.chain ((manifestPanel index).updater msg) )
  in
    Tuple.second <| List.foldl folder (0, (model, Cmd.none, [])) model.allManifests

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch <| List.indexedMap (\index _ -> (manifestPanel index).subscriptions model) model.panelModels

emptyModel = 
  { iiif = Iiif.Utils.empty
  , manifests = []
  , collection = Nothing
  , allManifests = []
  , selectedManifest = Nothing
  , panelModels = []
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    AddManifest manifestUri -> { model | manifests = model.manifests ++ [manifestUri]} |> updateAllManifests |> resetPanels |> U.noSideEffects
    ClearManifests -> { model | manifests = [] } |> updateAllManifests |> resetPanels |> U.noSideEffects
    SetManifests manifestUris -> { model | manifests = manifestUris } |> updateAllManifests |> resetPanels |> U.noSideEffects
    ClearCollection -> { model | collection = Nothing } |> updateAllManifests |> resetPanels |> U.noSideEffects
    SetCollection collectionUri -> { model | collection = Just collectionUri } |> updateAllManifests |> resetPanels |> U.noSideEffects
    IiifNotification notification -> 
      checkCollectionLoaded notification model
      |> U.chain (updateAllPanels (ManifestPanel.IiifNotification notification))
    ManifestClicked manifestUri -> (model, Cmd.none, [ManifestSelected manifestUri])
    CanvasClicked manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    ManifestPanelMsg index manifestPanelMsg -> 
      case ListE.getAt index model.panelModels of
        Just panelModel -> (manifestPanel index).updater manifestPanelMsg model
        Nothing -> (model, Cmd.none, [])
    
checkCollectionLoaded : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
checkCollectionLoaded notification model = 
  case notification of
    Iiif.Loading.CollectionLoaded iiif collectionUri -> 
      if model.collection == Just collectionUri then
        updateAllManifests model |> resetPanels |> U.noSideEffects
      else
        (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])


updateAllManifests : Model -> Model
updateAllManifests model =
  let 
    maybeCollection = Maybe.map (getCollection model.iiif) model.collection
    maybeManifestUris = Maybe.map .manifests maybeCollection
    collectionManifests = Maybe.withDefault [] maybeManifestUris
  in
    {model | allManifests = model.manifests ++ collectionManifests}


resetPanels : Model -> Model
resetPanels model = 
  let
    panelModel manifestUri = 
      let
        re = Maybe.withDefault Regex.never (Regex.fromString "\\W+")
        panelId = Regex.replace re (\_ -> "_") manifestUri
        manifest = getManifest model.iiif manifestUri
      in ManifestPanel.emptyModel |> ManifestPanel.id panelId |> ManifestPanel.manifest manifest
  in
  { model | panelModels = List.map panelModel model.allManifests }


view : Model -> Element Msg
view model = 
  Keyed.column [width fill, height fill, spacing 15] (List.indexedMap (\ind uri -> (uri, (manifestPanel ind).view model)) model.allManifests)
  