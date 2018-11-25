module ManifestList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, subscriptions)

import Json.Decode as Decode
import Regex
import List.Extra as ListE

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getCollection, isStub, manifestToString)
import Iiif.Loading
import Iiif.ImageApi

import Element exposing(..)

import IiifUI.ManifestPanel as ManifestPanel

import ManifestDetails
import Utils exposing(pluralise, wrapKey)
import Update as U


type alias Model = 
  { iiif : Iiif
  , manifests : List ManifestUri
  , collection : Maybe CollectionUri
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
          Just panelModel -> {panelModel | iiif = model.iiif}
          Nothing -> Debug.log "Panel model not found" ManifestPanel.emptyModel
    , wrapModel = \model subModel -> { model | panelModels = ListE.setAt ind { subModel | iiif = Iiif.Utils.empty } model.panelModels }
    , wrapMsg = ManifestPanelMsg ind
    , outEvaluator = \msgSub model ->
        case (msgSub, ListE.getAt ind (allManifestUris model)) of
          (_, Nothing) -> (model, Cmd.none, [])
          (ManifestPanel.ManifestSelected, Just manifestUri) -> (model, Cmd.none, [ManifestSelected manifestUri])
          (ManifestPanel.CanvasSelected canvasUri, Just manifestUri) -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch <| List.indexedMap (\index _ -> (manifestPanel index).subscriptions model) model.panelModels

emptyModel = 
  { iiif = Iiif.Utils.empty
  , manifests = []
  , collection = Nothing
  , selectedManifest = Nothing
  , panelModels = []
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model = 
  case msg of
    AddManifest manifestUri -> { model | manifests = model.manifests ++ [manifestUri]} |> updateLoadManifests |> U.mapModel resetPanels
    ClearManifests -> ({ model | manifests = [] }, Cmd.none, []) |> U.mapModel resetPanels
    SetManifests manifestUris -> { model | manifests = manifestUris } |> updateLoadManifests |> U.mapModel resetPanels
    ClearCollection -> ({ model | collection = Nothing }, Cmd.none, []) |> U.mapModel resetPanels
    SetCollection collectionUri -> updateLoadManifests { model | collection = Just collectionUri } |> U.mapModel resetPanels
    IiifNotification notification -> 
      case notification of
        Iiif.Loading.CollectionLoaded collectionUri -> 
          if model.collection == Just collectionUri then
            updateLoadManifests model |> U.mapModel resetPanels
          else
            (model, Cmd.none, [])
        _ -> (model, Cmd.none, [])
    ManifestClicked manifestUri -> (model, Cmd.none, [ManifestSelected manifestUri])
    CanvasClicked manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    ManifestPanelMsg index manifestPanelMsg -> 
      case ListE.getAt index model.panelModels of
        Just panelModel -> (manifestPanel index).updater manifestPanelMsg model
        Nothing -> (model, Cmd.none, [])
    

allManifestUris : Model -> List ManifestUri
allManifestUris model = 
  let 
    maybeCollection = Maybe.map (getCollection model.iiif) model.collection
    maybeManifestUris = Maybe.map .manifests maybeCollection
    collectionManifests = Maybe.withDefault [] maybeManifestUris
  in
    model.manifests ++ collectionManifests

resetPanels : Model -> Model
resetPanels model = 
  let
    panelModel manifestUri = 
      let
        re = Maybe.withDefault Regex.never (Regex.fromString "\\W+")
        panelId = Regex.replace re (\_ -> "_") manifestUri
      in ManifestPanel.emptyModel |> ManifestPanel.id panelId |> ManifestPanel.manifest manifestUri
  in
  { model | panelModels = List.map panelModel (allManifestUris model) }

updateLoadManifests : Model -> (Model, Cmd Msg, List OutMsg)
updateLoadManifests model = (model, Cmd.none, [])
-- Disabled in favour of lazy loading of manifests, keep code in case we want to
-- make lazy loading an option.
{-
  let 
    stubManifests = List.filter isStub (getManifests model.iiif (allManifestUris model))
    stubUris = List.map .id stubManifests
  in
    (model, Cmd.none, [])
      |> U.foldOut (\uri m -> [LoadManifest uri]) stubUris
-}

view : Model -> Element Msg
view model = 
  column [width fill, height fill, spacing 15] (List.indexedMap (\ind uri -> (manifestPanel ind).view model) (allManifestUris model))
  
