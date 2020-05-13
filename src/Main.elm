port module Main exposing(..)

import Browser
import Browser.Navigation as Nav

import Html as Html exposing(Html)
import Html.Attributes as Attributes exposing(style)

import Http
import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing(Set)

import UI.Core exposing(..)
import UI.Toast as Toast
import UI.TitleLine as TitleLine
import UI.Screen exposing(screen)

import Utils exposing(..)

import Iiif.Types exposing(..)
import Iiif.Loading exposing(loadManifest, loadCollection, loadAnnotationList, loadCollectionNextPage)
import Iiif.Utils exposing(getManifest, getCollection)
import UriMapper exposing (UriMapper)
import Update as U

import CollectionTree
import CollectionView
import ManifestView

port inPortLazyLoadManifest : (String -> msg) -> Sub msg

port inPortLazyLoadPagedCollection : (String -> msg) -> Sub msg

port inPortShowAnnotation : (Maybe AnnotationUri -> msg) -> Sub msg

port outPortScrollToView : Encode.Value -> Cmd msg

port outPortCopyToClipboard : List (String, String) -> Cmd msg

port inPortSetSelection : (Decode.Value -> msg) -> Sub msg

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , iiif : Iiif
  , collectionTreeModel : CollectionTree.Model
  , collectionViewModel : CollectionView.Model
  , manifestViewModel : ManifestView.Model
  , screen : Screen
  , uriMapper : UriMapper
  , errors : List String
  , iiifOptions : Iiif.Loading.Options
  }

type Screen = Browser | Viewer

type Msg
  = LinkClicked Browser.UrlRequest
  | LazyLoadManifest ManifestUri
  | LazyLoadPagedCollection CollectionUri
  | UrlChanged Url.Url
  | CollectionTreeMsg CollectionTree.Msg
  | CollectionViewMsg CollectionView.Msg
  | ManifestViewMsg ManifestView.Msg
  | IiifMsg Iiif.Loading.Msg
  | IiifNotification Iiif.Loading.Notification
  | CloseError Int
  | ShowAnnotation (Maybe AnnotationUri)
  | SetSelection Decode.Value

type OutMsg
  = LoadCollection CollectionUri
  | LoadCollectionPage CollectionUri
  | LoadManifest ManifestUri
  | LoadAnnotationList AnnotationListUri
  | CollectionSelected (List CollectionUri)
  | ManifestSelected ManifestUri
  | CanvasSelected ManifestUri CanvasUri
  | CanvasOpened ManifestUri CanvasUri
  | CloseViewer
  | RequestIiif (Iiif -> Msg)
  | ScrollToView ScrollInfo
  | CopyToClipboard (List (String, String))


collectionTree =
  U.subComponent 
    { component = CollectionTree.component 
    , unwrapModel = \model -> model.collectionTreeModel
    , wrapModel = \model subModel -> 
        if List.isEmpty subModel.errors then { model | collectionTreeModel = subModel }
        else {model | collectionTreeModel = { subModel | errors = []}, errors = model.errors ++ subModel.errors }
    , wrapMsg = CollectionTreeMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          CollectionTree.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          CollectionTree.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          CollectionTree.CollectionSelected path -> (model, Cmd.none, [CollectionSelected path])
          CollectionTree.ScrollToView scrollInfo -> (model, Cmd.none, [ScrollToView scrollInfo])
    }

collectionView =
  U.subComponent 
    { component = CollectionView.component 
    , unwrapModel = .collectionViewModel
    , wrapModel = \model subModel -> 
                      if List.isEmpty subModel.errors then {model | collectionViewModel = subModel }
                      else { model | collectionViewModel = { subModel | errors = []}, errors = model.errors ++ subModel.errors}
    , wrapMsg = CollectionViewMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          CollectionView.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          CollectionView.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          CollectionView.ManifestSelected uri -> (model, Cmd.none, [ManifestSelected uri])
          CollectionView.CanvasSelected manifestUri canvasUri -> (model, Cmd.none, [CanvasSelected manifestUri canvasUri])
          CollectionView.ScrollToView scrollInfo -> (model, Cmd.none, [ScrollToView scrollInfo])
    }

manifestView = 
  U.subComponent 
    { component = ManifestView.component 
    , unwrapModel = .manifestViewModel
    , wrapModel = \model subModel -> { model | manifestViewModel = subModel }
    , wrapMsg = ManifestViewMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestView.LoadManifest uri -> (model, Cmd.none, [LoadManifest uri])
          ManifestView.LoadCollection uri -> (model, Cmd.none, [LoadCollection uri])
          ManifestView.LoadAnnotationList uri -> (model, Cmd.none, [LoadAnnotationList uri])
          ManifestView.CanvasOpened manifestUri canvasUri -> (model, Cmd.none, [CanvasOpened manifestUri canvasUri])
          ManifestView.CloseViewer -> (model, Cmd.none, [CloseViewer])
          ManifestView.RequestIiif iiifMsg -> (model, Cmd.none, [RequestIiif (ManifestViewMsg << iiifMsg)])
          ManifestView.ScrollToView scrollInfo -> (model, Cmd.none, [ScrollToView scrollInfo])
          ManifestView.CopyToClipboard d -> (model, Cmd.none, [CopyToClipboard d])
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
  let
    decodedUriMapper = Decode.decodeValue (Decode.field "uriMapper" (UriMapper.uriMapperDecoder UriMapper.base)) flags
    setUriMapper = case decodedUriMapper of
      Result.Ok uriMapper -> U.mapModel (\m -> {m | uriMapper = uriMapper })
      Result.Err err -> U.mapModel (\m -> {m | errors = m.errors ++ [Decode.errorToString err] })
  in
  (emptyModel url key, Cmd.none, [])
    |> setUriMapper
    |> U.mapModel (\m -> { m | iiifOptions = Iiif.Loading.readOptions flags})
    |> U.chain (collectionTree.init flags)
    |> U.mapModel (\m -> { m | collectionTreeModel = CollectionTree.setContainerId "collection_tree_container" m.collectionTreeModel })
    |> U.chain (collectionView.init flags)
    |> U.mapModel (\m -> { m | collectionViewModel = CollectionView.setContainerId "collection_view_container" m.collectionViewModel })
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
          |> List.map model.uriMapper.inflate
          |> List.reverse
    (selectedManifest, selectedCanvas) = 
      case Maybe.map (String.split "/") viewFragment of
        Nothing -> (Nothing, Nothing)
        Just [] -> (Nothing, Nothing)
        Just [x] -> (Just (model.uriMapper.inflate x), Nothing)
        Just (x :: y :: xs) -> (Just (model.uriMapper.inflate x), Just (model.uriMapper.inflate y))
    selectedCollectionMaybe = Maybe.map (getCollection model.iiif) (List.head path)
    screen = case selectedManifest of
      Nothing -> Browser
      Just x -> Viewer
    newModel = { model | url = url, screen = screen }
  in
    collectionTree.updater (CollectionTree.SelectPath path) newModel
      |> U.chain (collectionView.updater (CollectionView.SetCollectionMaybe model.iiif selectedCollectionMaybe))
      |> U.chain (manifestView.updater (ManifestView.SetManifestAndCanvas (Maybe.map (getManifest model.iiif) selectedManifest) selectedCanvas))



updateUrl : Model -> (Model, Cmd Msg)
updateUrl model = 
  let
    treePath = 
      model.collectionTreeModel.selectedCollection
      |> List.reverse
      |> List.map model.uriMapper.deflate
      |> String.join "/"
    viewManifest = 
      [Maybe.map .id model.manifestViewModel.manifest, model.manifestViewModel.canvas]
      |> List.filterMap identity
      |> List.map model.uriMapper.deflate
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
  , iiif = Iiif.Utils.empty
  , collectionTreeModel = CollectionTree.emptyModel
  , collectionViewModel = CollectionView.emptyModel
  , manifestViewModel = ManifestView.emptyModel
  , screen = Browser
  , uriMapper = UriMapper.empty
  , errors = []
  , iiifOptions = Iiif.Loading.defaultOptions
  }

outMsgEvaluator : OutMsg -> Model -> (Model, Cmd Msg)
outMsgEvaluator msg model = 
  case msg of
    LoadManifest manifestUri ->
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadManifest model.iiifOptions manifestUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut    
    LoadCollection collectionUri -> 
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadCollection model.iiifOptions collectionUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut
    LoadCollectionPage collectionUri ->
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadCollectionNextPage model.iiifOptions collectionUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut
    LoadAnnotationList annotationListUri ->
      (model.iiif, Cmd.none, [])
        |> U.chain2 (loadAnnotationList model.iiifOptions annotationListUri)
        |> U.mapModel (\m -> {model | iiif = m})
        |> U.mapCmd IiifMsg
        |> U.ignoreOut
    CollectionSelected path ->
      let 
        collectionUriMaybe = List.head path
        collectionMaybe = Maybe.map (getCollection model.iiif) collectionUriMaybe
      in 
        collectionView.updater (CollectionView.SetCollectionMaybe model.iiif collectionMaybe) model
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    ManifestSelected uri ->
        manifestView.updater (ManifestView.SetManifest (Just <| getManifest model.iiif uri)) {model | screen = Viewer}
        |> U.chain2 updateUrl
        |> U.evalOut2 outMsgEvaluator
    CanvasSelected manifestUri canvasUri -> 
        manifestView.updater (ManifestView.SetManifestAndCanvas (Just <| getManifest model.iiif manifestUri) (Just canvasUri)) {model | screen = Viewer}
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
    RequestIiif iiifMsg ->
        update (iiifMsg model.iiif) model
    ScrollToView scrollInfo ->
        let
          axis = case scrollInfo.axis of
            ScrollX -> "x"
            ScrollY -> "y"
          alignment = case scrollInfo.alignment of
            ScrollStart -> "start"
            ScrollMiddle -> "middle"
          scrollTarget = case scrollInfo.target of
                            ScrollRef ref -> ("ref", Encode.string <| "#" ++ ref)
                            ScrollPos pos -> ("pos", Encode.int pos)
          scrollCmd = outPortScrollToView (Encode.object
            [ ("container", Encode.string <| "#" ++ scrollInfo.containerId)
            , scrollTarget
            , ("axis", Encode.string axis)
            , ("animate", Encode.bool scrollInfo.animate)
            , ("alignment", Encode.string alignment)
            ])
        in (model, scrollCmd)
    CopyToClipboard d -> (model, outPortCopyToClipboard d)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LazyLoadManifest manifestUri ->
      (model, Cmd.none, [LoadManifest manifestUri])
        |> U.evalOut2 outMsgEvaluator
    LazyLoadPagedCollection collectionUri ->
      (model, Cmd.none, [LoadCollectionPage collectionUri])
        |> U.evalOut2 outMsgEvaluator
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url -> 
      if url == model.url then (model, Cmd.none)
      else parseUrl url model |> U.evalOut2 outMsgEvaluator
    CloseError index ->
      ({model | errors = arrayRemove index model.errors}, Cmd.none)
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
    ShowAnnotation maybeAnnotationUri ->
      (model, Cmd.none, [])
        |> U.chain (manifestView.updater (ManifestView.ShowAnnotationPort model.iiif maybeAnnotationUri))
        |> U.evalOut2 outMsgEvaluator
    IiifMsg iiifMsg ->
      let (newModel, maybeNotification) = Iiif.Loading.update model.iiifOptions iiifMsg model
      in case maybeNotification of
        Just notification -> update (IiifNotification notification) newModel
        Nothing -> (newModel, Cmd.none)
    IiifNotification notification ->
      (model, Cmd.none, [])
        |> U.chain (collectionTree.updater (CollectionTree.IiifNotification notification))
        |> U.chain (collectionView.updater (CollectionView.IiifNotification notification))
        |> U.chain (manifestView.updater (ManifestView.IiifNotification notification))
        |> U.evalOut2 outMsgEvaluator
    SetSelection value -> 
      (model, Cmd.none, [])
        |> U.chain (manifestView.updater (ManifestView.SetSelection value))
        |> U.evalOut2 outMsgEvaluator


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ inPortLazyLoadManifest LazyLoadManifest
  , inPortLazyLoadPagedCollection LazyLoadPagedCollection
  , inPortShowAnnotation ShowAnnotation
  , collectionTree.subscriptions model
  , collectionView.subscriptions model
  , manifestView.subscriptions model
  , inPortSetSelection SetSelection
  ]


view : Model -> Browser.Document Msg
view model =
  { title = "Jalava"
  , body = 
    [ screen (model.screen == Browser) (browserView model)
    , screen (model.screen == Viewer) (manifestView.view model)
    , errorsView model
    , Html.div [ Attributes.style "display" "none" ] [ Html.div [ Attributes.id "annotation_overlay_wrapper"] [Html.div [Attributes.id "annotation_overlay"] []]]
    ]
  }

browserView : Model -> Html Msg
browserView model =
  row 0 [fullWidth, fullHeight]
    [ el [style "flex-grow" "1", fullHeight, style "flex-shrink" "0", style "flex-basis" "0", style "overflow" "scroll", Attributes.id "collection_tree_container"] (collectionTree.view model)
    , el [style "flex-grow" "2", fullHeight, style "flex-shrink" "0", style "flex-basis" "0", cssPadding <| cssPx 10, style "overflow-y" "scroll", Attributes.id "collection_view_container"] (collectionView.view model)
    ]

errorsView : Model -> Html Msg
errorsView model =
  let
    errorToast index content =
      Toast.error |> Toast.content (TitleLine.simple content) |> Toast.onClose (CloseError index) |> Toast.toast  
  in
    if List.length model.errors > 0 then
      column 15 
        [ cssPadding4 (cssPx 15) (cssPx 15) (cssPx 0) (cssPx 15)
        , Attributes.style "position" "absolute"
        , Attributes.style "top" "0"
        , Attributes.style "left" "0"
        , Attributes.style "right" "0"
        ] (List.indexedMap errorToast model.errors)
    else none
