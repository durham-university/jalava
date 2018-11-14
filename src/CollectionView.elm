module CollectionView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Url
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import ManifestList

import Update as U
import Config
import Utils exposing(iiifLink, pluralise, spinner)

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , collection : Maybe CollectionUri
  , manifestListModel : ManifestList.Model
  , errors : List String
  }

type Msg  = SetCollection (Maybe CollectionUri)
          | ManifestListMsg ManifestList.Msg
          | IiifNotification Iiif.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | ManifestSelected ManifestUri
            | CanvasSelected ManifestUri CanvasUri

manifestListSubModel : Model -> ManifestList.Model
manifestListSubModel model =
  let subModel = model.manifestListModel
  in { subModel | iiif = model.iiif }

manifestListOutMapper : ManifestList.OutMsg -> List OutMsg
manifestListOutMapper msg =
  case msg of
    ManifestList.LoadManifest uri -> [LoadManifest uri]
    ManifestList.LoadCollection uri -> [LoadCollection uri]
    ManifestList.ManifestSelected uri -> [ManifestSelected uri]
    ManifestList.CanvasSelected manifestUri canvasUri -> [CanvasSelected manifestUri canvasUri]

manifestListUpdater : ManifestList.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
manifestListUpdater msg model =
  ManifestList.update msg (manifestListSubModel model)
    |> manifestListPipe model

manifestListPipe : Model -> (ManifestList.Model, Cmd ManifestList.Msg, List ManifestList.OutMsg) -> (Model, Cmd Msg, List OutMsg)
manifestListPipe model = 
  U.mapCmd ManifestListMsg
  >> U.mapModel (\m -> { model | manifestListModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut manifestListOutMapper


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    ManifestList.init flags
      |> manifestListPipe baseModel


emptyModel : Model
emptyModel = 
  { iiif = Iiif.empty
  , collection = Nothing
  , manifestListModel = ManifestList.emptyModel
  , errors = []
  }


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    ManifestListMsg manifestListMsg -> manifestListUpdater manifestListMsg model
    SetCollection maybeCollectionUri -> 
      setManifestListCollection maybeCollectionUri { model | collection = maybeCollectionUri }
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (manifestListUpdater (ManifestList.IiifNotification notification))

setManifestListCollection : Maybe CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
setManifestListCollection maybeCollectionUri model = 
  let 
    maybeCollection = Maybe.map (getCollection model.iiif) maybeCollectionUri
    maybeManifestUris = Maybe.map .manifests maybeCollection
    manifestUris = Maybe.withDefault [] maybeManifestUris
  in
    case maybeCollectionUri of
      Just collectionUri -> manifestListUpdater (ManifestList.SetCollection collectionUri) model
      Nothing -> manifestListUpdater ManifestList.ClearCollection model


view : Model -> Html Msg
view model = 
  div [ class "collection_view" ] <|
    case model.collection of
      Just collectionUri ->
        let
          collection = getCollection model.iiif collectionUri
          manifestList = Html.map ManifestListMsg <| ManifestList.view (manifestListSubModel model)
          logoHtml = case collection.logo of
            Just logo -> [div [class "logo"] [ img [src logo] [] ] ]
            Nothing -> []
          spinnerHtml = case isStub collection of
            True -> [spinner]
            False -> []
        in
          [ Grid.row [Row.attrs [class "title_row"]] [ Grid.col [] (logoHtml ++ [h1 [] [ text <| collectionToString collection ]] ++ spinnerHtml ) ]
          , Grid.row [Row.attrs [class "info_row"]]  [ Grid.col [] [text <| pluralise (List.length collection.manifests) "manifest - " "manifests - ", iiifLink collectionUri]]
          , Grid.row [Row.attrs [class "manifests row"]] [ Grid.col [] [manifestList] ]
          ]
      Nothing -> []
