module CollectionView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component, setContainerId)

import Url
import Json.Decode as Decode

import UI.Core exposing(..)
import UI.Fonts exposing(..)
import UI.Collapsible as Collapsible
import UI.Colors as Colors
import UI.Button as Button
import UI.Error exposing(err)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy

import IiifUI.Spinner as Spinner
import IiifUI.CollectionDetails as CollectionDetails
import IiifUI.IiifLink as IiifLink

import ManifestList

import Update as U
import Utils exposing(pluralise, ScrollInfo, ScrollAlignment(..), ScrollAxis(..), ScrollTarget(..))

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getCollection, willLoad, collectionToString)
import Iiif.Loading
import Iiif.ImageApi as ImageApi

type alias Model =
  { collection : Maybe Collection
  , containerId : Maybe String
  , manifestListModel : ManifestList.Model
  , collapsible : Collapsible.Model
  , errors : List String
  }

type Msg  = SetCollection Iiif Collection
          | ClearCollection
          | SetCollectionMaybe Iiif (Maybe Collection)
          | ManifestListMsg ManifestList.Msg
          | IiifNotification Iiif.Loading.Notification
          | CollapsibleMsg Collapsible.Msg

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | ManifestSelected ManifestUri
            | CanvasSelected ManifestUri CanvasUri
            | ScrollToView ScrollInfo


manifestList =
  U.subComponent 
    { component = ManifestList.component 
    , unwrapModel = .manifestListModel
    , wrapModel = \model subModel -> 
        if List.isEmpty subModel.errors then { model | manifestListModel = subModel}
        else { model | manifestListModel = {subModel | errors = []}, errors = model.errors ++ subModel.errors}
    , wrapMsg = ManifestListMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestList.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          ManifestList.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          ManifestList.ManifestSelected uri -> (model, Cmd.none, [ManifestSelected uri])
          ManifestList.CanvasSelected manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }


collapsible = 
  U.subComponent 
    { component = Collapsible.component 
    , unwrapModel = .collapsible
    , wrapModel = \model subModel -> { model | collapsible = subModel }
    , wrapMsg = CollapsibleMsg
    , outEvaluator = \msgSub model -> (model, Cmd.none, [])
    }


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =  Sub.batch [ manifestList.subscriptions model, collapsible.subscriptions model ]


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    (emptyModel, Cmd.none, [])
      |> U.chain (manifestList.init flags)

setContainerId : String -> Model -> Model
setContainerId id_ model = {model | containerId = Just id_}

emptyModel : Model
emptyModel = 
  { collection = Nothing
  , manifestListModel = ManifestList.emptyModel
  , containerId = Nothing
  , collapsible = 
      Collapsible.emptyModel 
        |> Collapsible.closed 
        |> Collapsible.id "collection_details"
        |> Collapsible.labels (text "Show collection details") (text "Hide collection details")
        |> Collapsible.attributes [fullWidth, Attributes.style "flex-shrink" "1"]
  , errors = []
  }


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    CollapsibleMsg collapsibleMsg -> 
      (model, Cmd.none, []) |> U.chain (collapsible.updater collapsibleMsg)
    ManifestListMsg manifestListMsg -> manifestList.updater manifestListMsg model
    ClearCollection -> 
      {model | collection = Nothing} 
        |> manifestList.updater ManifestList.ClearCollection 
        |> U.mapModel updateCollectionInfo
    SetCollection iiif collection -> 
      { model | collection = Just collection } 
        |> loadCollection 
        |> U.chain (manifestList.updater (ManifestList.SetCollection iiif collection))
        |> U.mapModel updateCollectionInfo
        |> U.chain scrollToTop
    SetCollectionMaybe iiif maybeCollection ->
      case maybeCollection of
        Just collection -> update (SetCollection iiif collection) model
        Nothing -> update ClearCollection model
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (checkCollectionLoaded notification)
        |> U.chain (manifestList.updater (ManifestList.IiifNotification notification))

scrollToTop : Model -> ( Model, Cmd Msg, List OutMsg )
scrollToTop model = 
  case model.containerId of
    Nothing -> (model, Cmd.none, [])
    Just containerId -> (model, Cmd.none, [ScrollToView <| ScrollInfo containerId (ScrollPos 0) ScrollY False ScrollStart])

checkCollectionLoaded : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
checkCollectionLoaded notification model = 
  case notification of
    Iiif.Loading.CollectionLoaded iiif collectionUri -> 
      if Maybe.map .id model.collection == Just collectionUri then
        let collection = getCollection iiif collectionUri
        in 
          {model | collection = Just collection} |> U.noSideEffects |> U.mapModel updateCollectionInfo
      else
        (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])

updateCollectionInfo : Model -> Model
updateCollectionInfo model = 
  case model.collection of
    Just collection_ -> 
      let 
        collectionInfo = CollectionDetails.empty |> CollectionDetails.collection collection_ |> CollectionDetails.collectionDetails
      in 
        {model | collapsible = model.collapsible |> Collapsible.content collectionInfo }
    Nothing -> {model | collapsible = model.collapsible |> Collapsible.content none}


loadCollection : Model -> (Model, Cmd Msg, List OutMsg )
loadCollection model =
  case model.collection of
    Just collection ->
      if willLoad collection then (model, Cmd.none, [LoadCollection collection.id])
      else (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])

view : Model -> Html Msg
view model = Lazy.lazy view_ model

view_ : Model -> Html Msg
view_ model = 
  case model.collection of
    Just collection ->
      let
        logoElem = case collection.logo of
          Just logo -> Html.img [Attributes.height 60, Attributes.src (ImageApi.resourceUrlSimple (ImageApi.FitH 60) logo), Attributes.alt "logo"] []
          Nothing -> none
        spinnerElem = if willLoad collection then
                        Spinner.spinner
                      else
                        none

        errorElem = case collection.status of
                      Error e -> [err e]
                      _ -> []

        (collapsibleMsg, toggleLabel) = Collapsible.toggleButtonInfo model.collapsible
        toggleButton = 
          Button.slimLink 
          |> Button.color "Dim"
          |> Button.content (toggleLabel |> Html.map CollapsibleMsg)
          |> Button.onPress (CollapsibleMsg collapsibleMsg)
          |> Button.button

        allPagesLoaded = case collection.pageStatus of
          NoPages -> True
          IndexPage -> False
          MorePages -> False
          LastPage -> True

        countElement =
          if allPagesLoaded then
            el textBody (text <| pluralise (List.length collection.manifests) "manifest -" "manifests -")
          else
            el textBody (text <| pluralise (List.length collection.manifests) "manifest loaded (has more) -" "manifests loaded (has more) -")
      in
        column 0 [fullHeight, fullWidth] <|
          [ row 5 (textBody ++ [fullWidth, Attributes.style "font-size" "24px"]) [logoElem, text <| collectionToString collection, spinnerElem] ]
          ++ errorElem ++
          [ row 5 [fullWidth] [model.collapsible |> Collapsible.view |> Html.map CollapsibleMsg]
          , row 5 [fullWidth, cssColor <| Colors.toCss Colors.dimTextColor]
                  [ el [fullWidth, Attributes.style "flex-shrink" "1"] (toggleButton)
                  , countElement
                  , el [] (IiifLink.iiifLink collection.id)
                  ]
          , el [Attributes.style "padding-top" <| cssPx 10, Attributes.style "padding-bottom" <| cssPx 15, fullHeight, fullWidth] (manifestList.view model)
          ]
    Nothing -> column 0 [fullHeight, fullWidth] []
 
