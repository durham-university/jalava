module CollectionView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode

import UI.Core exposing(..)
import UI.Fonts exposing(..)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events

import IiifUI.Spinner as Spinner
import IiifUI.IiifLink exposing(iiifLink)

import ManifestList

import Update as U
import Utils exposing(pluralise)

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getCollection, isStub, collectionToString)
import Iiif.Loading

type alias Model =
  { iiif : Iiif
  , collection : Maybe CollectionUri
  , manifestListModel : ManifestList.Model
  , errors : List String
  }

type Msg  = SetCollection (Maybe CollectionUri)
          | ManifestListMsg ManifestList.Msg
          | IiifNotification Iiif.Loading.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | ManifestSelected ManifestUri
            | CanvasSelected ManifestUri CanvasUri


manifestList =
  U.subComponent 
    { component = ManifestList.component 
    , unwrapModel = \model -> let subModel = model.manifestListModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | manifestListModel = { subModel | iiif = Iiif.Utils.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = ManifestListMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestList.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          ManifestList.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          ManifestList.ManifestSelected uri -> (model, Cmd.none, [ManifestSelected uri])
          ManifestList.CanvasSelected manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =  manifestList.subscriptions model


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    (emptyModel, Cmd.none, [])
      |> U.chain (manifestList.init flags)


emptyModel : Model
emptyModel = 
  { iiif = Iiif.Utils.empty
  , collection = Nothing
  , manifestListModel = ManifestList.emptyModel
  , errors = []
  }


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    ManifestListMsg manifestListMsg -> manifestList.updater manifestListMsg model
    SetCollection maybeCollectionUri -> 
      setManifestListCollection maybeCollectionUri { model | collection = maybeCollectionUri }
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (manifestList.updater (ManifestList.IiifNotification notification))

setManifestListCollection : Maybe CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
setManifestListCollection maybeCollectionUri model = 
  let 
    maybeCollection = Maybe.map (getCollection model.iiif) maybeCollectionUri
    maybeManifestUris = Maybe.map .manifests maybeCollection
    manifestUris = Maybe.withDefault [] maybeManifestUris
  in
    case maybeCollectionUri of
      Just collectionUri -> manifestList.updater (ManifestList.SetCollection collectionUri) model
      Nothing -> manifestList.updater ManifestList.ClearCollection model


view : Model -> Html Msg
view model = 
  case model.collection of
    Just collectionUri ->
      let
        collection = getCollection model.iiif collectionUri
        logoElem = case collection.logo of
          Just logo -> Html.img [Attributes.height 60, Attributes.src logo, Attributes.alt "logo"] []
          Nothing -> none
        spinnerElem = case isStub collection of
          True -> Spinner.spinner
          False -> none
      in
        column 0 [fullHeight, fullWidth]
          [ row 5 (textBody ++ [fullWidth, Attributes.style "font-size" "24px"]) [logoElem, text <| collectionToString collection, spinnerElem]
          , row 5 [fullWidth, Attributes.style "justify-content" "flex-end"]
            [ el textBody (text <| pluralise (List.length collection.manifests) "manifest - " "manifests - ")
            , el [] (iiifLink collectionUri)
            ]
          , el [Attributes.style "padding-top" <| cssPx 10, Attributes.style "padding-bottom" <| cssPx 15, fullHeight, fullWidth] (manifestList.view model)
          ]
    Nothing -> none
 