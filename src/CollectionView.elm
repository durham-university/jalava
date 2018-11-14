module CollectionView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

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


manifestList =
  U.subComponent 
    { component = ManifestList.component 
    , unwrapModel = \model -> let subModel = model.manifestListModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | manifestListModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = ManifestListMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestList.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          ManifestList.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          ManifestList.ManifestSelected uri -> (model, Cmd.none, [ManifestSelected uri])
          ManifestList.CanvasSelected manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    (emptyModel, Cmd.none, [])
      |> U.chain (manifestList.init flags)


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
  div [ class "collection_view" ] <|
    case model.collection of
      Just collectionUri ->
        let
          collection = getCollection model.iiif collectionUri
          logoHtml = case collection.logo of
            Just logo -> [div [class "logo"] [ img [src logo] [] ] ]
            Nothing -> []
          spinnerHtml = case isStub collection of
            True -> [spinner]
            False -> []
        in
          [ Grid.row [Row.attrs [class "title_row"]] [ Grid.col [] (logoHtml ++ [h1 [] [ text <| collectionToString collection ]] ++ spinnerHtml ) ]
          , Grid.row [Row.attrs [class "info_row"]]  [ Grid.col [] [text <| pluralise (List.length collection.manifests) "manifest - " "manifests - ", iiifLink collectionUri]]
          , Grid.row [Row.attrs [class "manifests row"]] [ Grid.col [] [manifestList.view model] ]
          ]
      Nothing -> []
