{-
  TODO: 
   - Might want to change manipulators that now are Model -> something -> (Model, Cmd)
     to instead be something -> Model -> (Model, Cmd) then they'd be easier to chain
   - Not sure why tree doesn't open or select collections properly, refactoring
     previous point may help
   - Currently problems with loading stubs when needed, see next point
   - Consider somehow processing all IiifMsgs at the top level and then bubbling messages
     to child components

-}

module Main exposing(..)

import Debug

import Browser
import Browser.Navigation as Nav

import Http
import Url
import Json.Decode as Decode
import Set exposing(Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert

import Utils exposing(updateWith)

import Iiif exposing (..)
import Config
import Update as U

import CollectionTree
import CollectionView


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , iiif : Iiif
  , collectionTreeModel : CollectionTree.Model
  , collectionViewModel : CollectionView.Model
  , errors : List String
  }


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | CollectionTreeMsg CollectionTree.Msg
  | CollectionViewMsg CollectionView.Msg
  | IiifMsg Iiif.Msg
  | IiifNotification Iiif.Notification

type OutMsg
  = LoadCollection CollectionUri
  | LoadManifest ManifestUri


collectionTreeSubModel : Model -> CollectionTree.Model
collectionTreeSubModel model =
  let subModel = model.collectionTreeModel
  in { subModel | iiif = model.iiif }

collectionTreeOutMapper : CollectionTree.OutMsg -> OutMsg
collectionTreeOutMapper msg =
  case msg of
    CollectionTree.LoadManifest uri -> LoadManifest uri
    CollectionTree.LoadCollection uri -> LoadCollection uri

collectionTreeUpdater : CollectionTree.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
collectionTreeUpdater msg model =
  CollectionTree.update msg (collectionTreeSubModel model)
    |> (collectionTreePipe model)

collectionTreePipe : Model -> (CollectionTree.Model, Cmd CollectionTree.Msg, List CollectionTree.OutMsg) -> (Model, Cmd Msg, List OutMsg)
collectionTreePipe model = 
  U.mapCmd CollectionTreeMsg 
  >> U.mapModel (\m -> { model | collectionTreeModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut collectionTreeOutMapper


collectionViewSubModel : Model -> CollectionView.Model
collectionViewSubModel model =
  let subModel = model.collectionViewModel
  in { subModel | iiif = model.iiif }

collectionViewOutMapper : CollectionView.OutMsg -> OutMsg
collectionViewOutMapper msg =
  case msg of
    CollectionView.LoadManifest uri -> LoadManifest uri
    CollectionView.LoadCollection uri -> LoadCollection uri

collectionViewUpdater : CollectionView.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
collectionViewUpdater msg model =
  CollectionView.update msg (collectionViewSubModel model)
    |> (collectionViewPipe model)

collectionViewPipe : Model -> (CollectionView.Model, Cmd CollectionView.Msg, List CollectionView.OutMsg) -> (Model, Cmd Msg, List OutMsg)
collectionViewPipe model = 
  U.mapCmd CollectionViewMsg 
  >> U.mapModel (\m -> { model | collectionViewModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut collectionViewOutMapper


main : Program Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let 
    (collectionTree, collectionTreeCmd, collectionTreeOut) = CollectionTree.init flags url
    (collectionView, collectionViewCmd, collectionViewOut) = CollectionView.init flags url
    baseModel = emptyModel url key
  in
    CollectionTree.init flags url
      |> collectionTreePipe baseModel
      |> U.chain 
        (\m -> CollectionView.init flags url 
                |> collectionViewPipe m )
      |> U.evalOut outMsgEvaluator


emptyModel : Url.Url -> Nav.Key -> Model
emptyModel url key = 
  { key = key
  , url = url
  , iiif = Iiif.empty
  , collectionTreeModel = CollectionTree.emptyModel url
  , collectionViewModel = CollectionView.emptyModel url
  , errors = []
  }

outMsgEvaluator : OutMsg -> Model -> (Model, Cmd Msg)
outMsgEvaluator msg model = 
  case msg of
    LoadManifest manifestUri ->
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadManifest manifestUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut    
    LoadCollection collectionUri -> 
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadCollection collectionUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url -> 
      (model, Cmd.none, [])
        |> U.chain (collectionTreeUpdater (CollectionTree.UrlChanged url))
        |> U.chain (collectionViewUpdater (CollectionView.UrlChanged url))
        |> U.evalOut outMsgEvaluator
    CollectionTreeMsg collectionTreeMsg ->
      (model, Cmd.none, [])
        |> U.chain (collectionTreeUpdater collectionTreeMsg)
        |> U.evalOut outMsgEvaluator
    CollectionViewMsg collectionViewMsg ->
      (model, Cmd.none, [])
        |> U.chain (collectionViewUpdater collectionViewMsg)
        |> U.evalOut outMsgEvaluator
    IiifMsg iiifMsg ->
      let (newModel, maybeNotification) = Iiif.update iiifMsg model
      in case maybeNotification of
        Just notification -> update (IiifNotification notification) newModel
        Nothing -> (newModel, Cmd.none)
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (collectionTreeUpdater (CollectionTree.IiifNotification notification))
        |> U.chain (collectionViewUpdater (CollectionView.IiifNotification notification))
        |> U.evalOut outMsgEvaluator


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


view : Model -> Browser.Document Msg
view model =
  let
    collectionTree = Html.map CollectionTreeMsg <| CollectionTree.view (collectionTreeSubModel model)
    collectionView = Html.map CollectionViewMsg <| CollectionView.view (collectionViewSubModel model)
  in
  { title = "Elm IIIF"
  , body = 
    [ div []
      [ Grid.containerFluid [] <|
        (List.map (\e -> Alert.simpleDanger [] [text ("Error: " ++ e)]) model.errors)
        ++
        [ Grid.row []
          [ Grid.col [ Col.xs4, Col.attrs [ class "col_collection_tree" ] ] [ collectionTree ]
          , Grid.col [ Col.xs8, Col.attrs [ class "col_collection_view" ] ] [ collectionView ]
          ]
        ]
      ]
    ]
  }
