module CollectionView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode

import UI.Core exposing(..)
import UI.Fonts exposing(..)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy

import IiifUI.Spinner as Spinner
import IiifUI.IiifLink exposing(iiifLink)

import ManifestList

import Update as U
import Utils exposing(pluralise)

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, getCollection, isStub, collectionToString)
import Iiif.Loading

type alias Model =
  { collection : Maybe Collection
  , manifestListModel : ManifestList.Model
  , errors : List String
  }

type Msg  = SetCollection Iiif Collection
          | ClearCollection
          | SetCollectionMaybe Iiif (Maybe Collection)
          | ManifestListMsg ManifestList.Msg
          | IiifNotification Iiif.Loading.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | ManifestSelected ManifestUri
            | CanvasSelected ManifestUri CanvasUri


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
  { collection = Nothing
  , manifestListModel = ManifestList.emptyModel
  , errors = []
  }


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    ManifestListMsg manifestListMsg -> manifestList.updater manifestListMsg model
    ClearCollection -> 
      {model | collection = Nothing} |> manifestList.updater ManifestList.ClearCollection 
    SetCollection iiif collection -> 
      { model | collection = Just collection } 
        |> loadCollection 
        |> U.chain (manifestList.updater (ManifestList.SetCollection iiif collection))
    SetCollectionMaybe iiif maybeCollection ->
      case maybeCollection of
        Just collection -> update (SetCollection iiif collection) model
        Nothing -> update ClearCollection model
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (checkCollectionLoaded notification)
        |> U.chain (manifestList.updater (ManifestList.IiifNotification notification))

checkCollectionLoaded : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
checkCollectionLoaded notification model = 
  case notification of
    Iiif.Loading.CollectionLoaded iiif collectionUri -> 
      if Maybe.map .id model.collection == Just collectionUri then
        let collection = getCollection iiif collectionUri
        in 
          {model | collection = Just collection} |> U.noSideEffects
      else
        (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])


loadCollection : Model -> (Model, Cmd Msg, List OutMsg )
loadCollection model =
  case model.collection of
    Just collection ->
      if isStub collection then (model, Cmd.none, [LoadCollection collection.id])
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
            , el [] (iiifLink collection.id)
            ]
          , el [Attributes.style "padding-top" <| cssPx 10, Attributes.style "padding-bottom" <| cssPx 15, fullHeight, fullWidth] (manifestList.view model)
          ]
    Nothing -> column 0 [fullHeight, fullWidth] []
 