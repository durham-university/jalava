module ManifestList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, subscriptions)

import Json.Decode as Decode
import Regex
import List.Extra as ListE

import UI.Core exposing(..)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getManifests, getCollection, manifestToString)
import Iiif.Loading
import Iiif.ImageApi

import IiifUI.ManifestPanel as ManifestPanel
import IiifUI.Spinner as Spinner

import Utils exposing(pluralise, wrapKey)
import Update as U


type alias Model = 
  { manifests : List Manifest
  , collection : Maybe Collection
  , selectedManifest : Maybe ManifestUri
  , panelModels : List (ManifestPanel.Model)
  , manifestLinkers : List (ManifestPanel.ManifestLinker)
  , errors : List String
  }

type Msg = AddManifest Manifest
         | ClearManifests
         | SetManifests (List Manifest)
         | SetCollection Iiif Collection
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
        case (msgSub, ListE.getAt ind model.manifests) of
          (_, Nothing) -> (model, Cmd.none, [])
          (ManifestPanel.ManifestSelected, Just manifest) -> (model, Cmd.none, [ManifestSelected manifest.id])
          (ManifestPanel.CanvasSelected canvasUri, Just manifest) -> (model, Cmd.none, [CanvasSelected manifest.id canvasUri])
    }

updateAllPanels : ManifestPanel.Msg -> Model -> (Model, Cmd Msg, List OutMsg )
updateAllPanels msg model =
  let
    folder : a -> (Int, (Model, Cmd Msg, List OutMsg)) -> (Int, (Model, Cmd Msg, List OutMsg))
    folder _ (index, result) =
      (index + 1, result |> U.chain ((manifestPanel index).updater msg) )
  in
    Tuple.second <| List.foldl folder (0, (model, Cmd.none, [])) model.manifests

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags =
  let
    decodedLinkers = Decode.decodeValue (Decode.maybe (Decode.field "manifestLinks" ManifestPanel.manifestLinkersDecoder) |> Decode.map (Maybe.withDefault [])) flags
    setLinkers = case decodedLinkers of
      Result.Ok linkers -> U.mapModel (\m -> {m | manifestLinkers = linkers })
      Result.Err err -> U.mapModel (\m -> {m | errors = m.errors ++ [Decode.errorToString err] })
  in
  (emptyModel, Cmd.none, [])
  |> setLinkers

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch <| List.indexedMap (\index _ -> (manifestPanel index).subscriptions model) model.panelModels

emptyModel = 
  { manifests = []
  , collection = Nothing
  , selectedManifest = Nothing
  , panelModels = []
  , manifestLinkers = []
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    AddManifest manifest -> 
      let
        newModel = case model.collection of
                      Nothing -> { model | manifests = model.manifests ++ [manifest] }
                      Just _ -> {model | manifests = [manifest], collection = Nothing}
      in
        newModel |> resetPanels |> U.noSideEffects
    ClearManifests -> 
      case model.collection of
        Nothing -> { model | manifests = [] } |> resetPanels |> U.noSideEffects
        Just _ -> model |> U.noSideEffects
    SetManifests manifests -> { model | manifests = manifests, collection = Nothing } |> resetPanels |> U.noSideEffects
    ClearCollection -> 
      case model.collection of
        Nothing -> model |> U.noSideEffects
        Just _ -> { model | manifests = [], collection = Nothing } |> resetPanels |> U.noSideEffects
    SetCollection iiif collection -> 
      ({ model | collection = Just collection }, Cmd.none, [])
      |> U.mapModel (updateCollectionManifests iiif)
      |> U.mapModel (resetPanels)
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
      if Maybe.map .id model.collection == Just collectionUri then
        let collection = getCollection iiif collectionUri
        in 
          {model | collection = Just collection}
            |> updateCollectionManifests iiif 
            |> resetPanels 
            |> U.noSideEffects
      else
        (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])


updateCollectionManifests : Iiif -> Model -> Model
updateCollectionManifests iiif model =
  let 
    maybeManifestUris = Maybe.map .manifests model.collection
    maybeManifests = Maybe.map (getManifests iiif) maybeManifestUris
    collectionManifests = Maybe.withDefault [] maybeManifests
  in
    {model | manifests = collectionManifests}


resetPanels : Model -> Model
resetPanels model = 
  let
    panelModel manifest = 
      let
        re = Maybe.withDefault Regex.never (Regex.fromString "\\W+")
        panelId = Regex.replace re (\_ -> "_") manifest.id
      in ManifestPanel.emptyModel |> ManifestPanel.id panelId |> ManifestPanel.manifest manifest |> ManifestPanel.linkers model.manifestLinkers
  in
  { model | panelModels = List.map panelModel model.manifests }

view : Model -> Html Msg
view model = Lazy.lazy view_ model

view_ : Model -> Html Msg
view_ model = 
  let
    pageLoader = case model.collection of
      Nothing -> []
      Just collection ->
        let
          loaderDiv = [Html.div [Attributes.class "lazyload collection_page_lazyload", Attributes.attribute "data-collection-uri" collection.id] [Spinner.spinnerThumbnail]]
        in
          case collection.pageStatus of
            NoPages -> []
            LastPage -> []
            IndexPage -> loaderDiv
            MorePages -> loaderDiv
  in
    column 15 [fullWidth, fullHeight] <| (List.indexedMap (\ind uri -> (manifestPanel ind).view model) model.manifests) ++ pageLoader
  