module Main exposing(..)

import Browser
import Browser.Navigation as Nav

import Http
import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing(Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert

import Utils exposing(..)

import Iiif exposing (..)
import Config
import Update as U

import CollectionTree
import CollectionView
import ManifestView

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , iiif : Iiif
  , collectionTreeModel : CollectionTree.Model
  , collectionViewModel : CollectionView.Model
  , manifestViewModel : ManifestView.Model
  , screen : Screen
  , errors : List String
  }

type Screen = Browser | Viewer

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | CollectionTreeMsg CollectionTree.Msg
  | CollectionViewMsg CollectionView.Msg
  | ManifestViewMsg ManifestView.Msg
  | IiifMsg Iiif.Msg
  | IiifNotification Iiif.Notification
  | AlertMsg Int Alert.Visibility

type OutMsg
  = LoadCollection CollectionUri
  | LoadManifest ManifestUri
  | CollectionSelected (List CollectionUri)
  | ManifestSelected ManifestUri
  | CanvasSelected ManifestUri CanvasUri
  | CanvasOpened ManifestUri CanvasUri
  | CloseViewer


collectionTree =
  U.subComponent 
    { component = CollectionTree.component 
    , unwrapModel = \model -> let subModel = model.collectionTreeModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | collectionTreeModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = CollectionTreeMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          CollectionTree.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          CollectionTree.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          CollectionTree.CollectionSelected path -> (model, Cmd.none, [CollectionSelected path])
    }

collectionView =
  U.subComponent 
    { component = CollectionView.component 
    , unwrapModel = \model -> let subModel = model.collectionViewModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | collectionViewModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = CollectionViewMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          CollectionView.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          CollectionView.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          CollectionView.ManifestSelected uri -> (model, Cmd.none, [ManifestSelected uri])
          CollectionView.CanvasSelected manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
    }

manifestView = 
  U.subComponent 
    { component = ManifestView.component 
    , unwrapModel = \model -> let subModel = model.manifestViewModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | manifestViewModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = ManifestViewMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestView.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          ManifestView.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          ManifestView.CanvasOpened manifestUri canvasUri -> (model, Cmd.none, [CanvasOpened manifestUri canvasUri])
          ManifestView.CloseViewer -> (model, Cmd.none, [CloseViewer])
    }


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
  (emptyModel url key, Cmd.none, [])
    |> U.chain (collectionTree.init flags)
    |> U.chain (collectionView.init flags)
    |> U.chain (manifestView.init flags)
    |> U.chain (parseUrl url)
    |> U.evalOut2 outMsgEvaluator


parseUrl : Url.Url -> Model -> (Model, Cmd Msg, List OutMsg)
parseUrl url model =
  let 
    (pathFragment, viewFragment) =
      case Maybe.map (String.split "!") url.fragment of
        Nothing -> (Nothing, Nothing)
        Just [] -> (Nothing, Nothing)
        Just [x] -> (Just x, Nothing)
        Just (x :: y :: xs) -> (Just x, Just y)
    path =
      case pathFragment of
        Nothing -> []
        Just fragment ->
          String.split "/" fragment
          |> List.map Config.completeUri
          |> List.reverse
    (selectedManifest, selectedCanvas) = 
      case Maybe.map (String.split "/") viewFragment of
        Nothing -> (Nothing, Nothing)
        Just [] -> (Nothing, Nothing)
        Just [x] -> (Just (Config.completeUri x), Nothing)
        Just (x :: y :: xs) -> (Just (Config.completeUri x), Just (Config.completeCanvasUri (Config.completeUri x) y))
    screen = case selectedManifest of
      Nothing -> Browser
      Just x -> Viewer
    newModel = { model | url = url, screen = screen }
  in
    collectionTree.updater (CollectionTree.SelectPath path) newModel
      |> U.chain (collectionView.updater (CollectionView.SetCollection (List.head path)))
      |> U.chain (manifestView.updater (ManifestView.SetManifestAndCanvas selectedManifest selectedCanvas))



updateUrl : Model -> (Model, Cmd Msg)
updateUrl model = 
  let
    treePath = 
      model.collectionTreeModel.selectedCollection
      |> List.reverse
      |> List.map Config.shortenUri
      |> String.join "/"
    viewManifest = 
      [model.manifestViewModel.manifest, model.manifestViewModel.canvas]
      |> List.filterMap identity
      |> List.map Config.shortenUri 
      |> String.join "/"
    newFragment = 
      case model.screen of
        Browser -> treePath
        Viewer -> treePath ++ "!" ++ viewManifest
    oldUrl = model.url
    newUrl = { oldUrl | fragment = Just newFragment }
  in ({ model | url = newUrl }, Nav.pushUrl model.key (Url.toString newUrl))


emptyModel : Url.Url -> Nav.Key -> Model
emptyModel url key = 
  { key = key
  , url = url
  , iiif = Iiif.empty
  , collectionTreeModel = CollectionTree.emptyModel
  , collectionViewModel = CollectionView.emptyModel
  , manifestViewModel = ManifestView.emptyModel
  , screen = Browser
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
    CollectionSelected path ->
      let collectionUri = List.head path
      in 
        collectionView.updater (CollectionView.SetCollection collectionUri) model
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    ManifestSelected uri ->
        manifestView.updater (ManifestView.SetManifest (Just uri)) {model | screen = Viewer}
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    CanvasSelected manifestUri canvasUri -> 
        manifestView.updater (ManifestView.SetManifestAndCanvas (Just manifestUri) (Just canvasUri)) {model | screen = Viewer}
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    CloseViewer -> 
        ({model | screen = Browser}, Cmd.none, [])
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    CanvasOpened manifestUri canvasUri -> 
        (model, Cmd.none, [])
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator


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
      if url == model.url then (model, Cmd.none)
      else parseUrl url model |> U.evalOut2 outMsgEvaluator
    AlertMsg index visibility->
      if visibility == Alert.closed then
        ({model | errors = arrayRemove index model.errors}, Cmd.none)
      else (model, Cmd.none)
    CollectionTreeMsg collectionTreeMsg ->
      (model, Cmd.none, [])
        |> U.chain (collectionTree.updater collectionTreeMsg)
        |> U.evalOut2 outMsgEvaluator
    CollectionViewMsg collectionViewMsg ->
      (model, Cmd.none, [])
        |> U.chain (collectionView.updater collectionViewMsg)
        |> U.evalOut2 outMsgEvaluator
    ManifestViewMsg manifestViewMsg ->
      (model, Cmd.none, [])
        |> U.chain (manifestView.updater manifestViewMsg)
        |> U.evalOut2 outMsgEvaluator
    IiifMsg iiifMsg ->
      let (newModel, maybeNotification) = Iiif.update iiifMsg model
      in case maybeNotification of
        Just notification -> update (IiifNotification notification) newModel
        Nothing -> (newModel, Cmd.none)
    IiifNotification notification ->
      let 
        _ = case notification of 
          Iiif.ManifestLoaded uri -> uri
          Iiif.CollectionLoaded uri -> uri
      in
      (model, Cmd.none, [])
        |> U.chain (collectionTree.updater (CollectionTree.IiifNotification notification))
        |> U.chain (collectionView.updater (CollectionView.IiifNotification notification))
        |> U.chain (manifestView.updater (ManifestView.IiifNotification notification))
        |> U.evalOut2 outMsgEvaluator


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


alertDialog : Int -> String -> Html Msg
alertDialog index message = 
  Alert.config
    |> Alert.danger
    |> Alert.dismissable (AlertMsg index)
    |> Alert.children [text message]
    |> Alert.view Alert.shown

view : Model -> Browser.Document Msg
view model =
  let
    browserHide = if model.screen == Browser then "" else " hide"
    manifestViewHide = if model.screen == Viewer then "" else " hide"
  in
  { title = "Elm IIIF"
  , body = 
    [ div [ class <| "manifest_browser_wrapper" ++ browserHide]
        [ Grid.containerFluid [] 
          [ Grid.row []
            [ Grid.col [ Col.xs4, Col.attrs [ class "col_collection_tree" ] ] [ collectionTree.view model ]
            , Grid.col [ Col.xs8, Col.attrs [ class "col_collection_view" ] ] [ collectionView.view model ]
            ]
          ]
        ]
    , div [ class <| "manifest_view_wrapper" ++ manifestViewHide] [ manifestView.view model ]
    , div [ class "error_overlay" ] (List.indexedMap alertDialog model.errors)
    ]
  }
