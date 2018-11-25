port module ManifestView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode

import Html
import Html.Attributes

import Element exposing(..)
import Element.Lazy as Lazy

import UI.Button as Button
import UI.TitleLine as TitleLine
import IiifUI.Spinner as Spinner
import IiifUI.ManifestTitle as ManifestTitle

import Update as U
import Utils exposing(pluralise, flip)

import CanvasList
import ManifestMenu
import AnnotationView exposing(..)

import Iiif.Types exposing(..)
import Iiif.Encoders
import Iiif.Loading
import Iiif.Utils exposing(getManifest, getCanvas, getRange, isStub, manifestToString, getCanvasAnnotationLists, getAnnotationLists, getCanvasAnnotation)
import Iiif.ImageApi exposing(osdSource)

port outPortOsdCmd : Encode.Value -> Cmd msg

port outPortSetAnnotationsCmd : Encode.Value -> Cmd msg

type alias Model =
  { manifest : Maybe Manifest
  , canvas : Maybe CanvasUri
  , canvasListModel : CanvasList.Model
  , annotation : Maybe Annotation
  , osdElemId : String
  , menuModel : ManifestMenu.Model
  }


type Msg  = SetManifest (Maybe Manifest)
          | SetCanvas (Maybe CanvasUri)
          | SetManifestAndCanvas (Maybe Manifest) (Maybe CanvasUri)
          | CanvasListMsg CanvasList.Msg
          | ManifestMenuMsg ManifestMenu.Msg
          | IiifNotification Iiif.Loading.Notification
          | SetMenuOpen Bool
          | CloseClicked
          | ShowAnnotationPort Iiif (Maybe AnnotationUri)
          | SetOverlayInOsd Iiif

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | LoadAnnotationList AnnotationListUri
            | CanvasOpened ManifestUri CanvasUri
            | CloseViewer
            | RequestIiif (Iiif -> Msg)


canvasList =
  U.subComponent 
    { component = CanvasList.component 
    , unwrapModel = .canvasListModel
    , wrapModel = \model subModel -> { model | canvasListModel = subModel }
    , wrapMsg = CanvasListMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          CanvasList.CanvasOpened uri -> 
            case model.manifest of
              Nothing -> (model, Cmd.none, [])
              Just manifestUri -> update (SetCanvas (Just uri)) model
    }

manifestMenu = 
  U.subComponent 
    { component = ManifestMenu.component 
    , unwrapModel = .menuModel
    , wrapModel = \model subModel -> { model | menuModel = subModel }
    , wrapMsg = ManifestMenuMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestMenu.RangeSelected rangeUri ->
            let
              maybeRange = Maybe.andThen (flip getRange <| rangeUri) model.manifest
              maybeCanvasUri = Maybe.andThen (List.head) (Maybe.map .canvases maybeRange)
            in
              case maybeCanvasUri of
                Just canvasUri -> update (SetCanvas (Just canvasUri)) model
                Nothing -> (model, Cmd.none, [])
    }


openCanvasInOsd : Model -> (Model, Cmd Msg, List OutMsg)
openCanvasInOsd model = 
  let
    maybeCanvas = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing
    maybeSource = Maybe.andThen osdSource maybeCanvas
    maybeCmd = Maybe.map 
      (\source -> 
        outPortOsdCmd (Encode.object 
          [ ("type", Encode.string "setSource")
          , ("for", Encode.string model.osdElemId)
          , ("value", Encode.string source)
          ])
      ) maybeSource
  in
    case (maybeCmd, Maybe.map .id model.manifest, model.canvas) of 
      (Just cmd, Just manifestUri, Just canvasUri) -> (model, cmd, [CanvasOpened manifestUri canvasUri])
      _ -> (model, Cmd.none, [])


setOverlayInOsd : Iiif -> Model -> (Model, Cmd Msg, List OutMsg)
setOverlayInOsd iiif model =
  let
    maybeCanvas = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing

    annotations : List Annotation
    annotations = maybeCanvas
                    |> Maybe.map getCanvasAnnotationLists
                    |> Maybe.map (getAnnotationLists iiif)
                    |> Maybe.withDefault []
                    |> List.filter (not << isStub)
                    |> List.concatMap .annotations
                    |> List.filter (\a -> a.on |> Maybe.map .selector |> (/=) Nothing)
-- this check disabled temporarily due to a bug in Trifle
--                    |> List.filter (\a -> a.on |> Maybe.map .full |> (==) (Just canvasUri))

    encoder : Maybe Canvas -> List Annotation -> Encode.Value
    encoder c a = 
      let
        encodedAnnotations = Encode.list Iiif.Encoders.annotationEncoder annotations
      in
      case c of
        Just ca -> Encode.object [("canvas", Iiif.Encoders.canvasEncoder ca), ("annotations", encodedAnnotations)]
        Nothing -> Encode.object [("canvas", Encode.null), ("annotations", encodedAnnotations)]

  in (model, outPortSetAnnotationsCmd (encoder maybeCanvas annotations), [])

scrollCanvasLine : Bool -> Model -> (Model, Cmd Msg, List OutMsg)
scrollCanvasLine animate model =
  case model.canvas of
    Nothing -> (model, Cmd.none, [])
    Just canvasUri -> canvasList.updater (CanvasList.ScrollToView canvasUri animate) model


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  (emptyModel, Cmd.none, [])
    |> U.chain (canvasList.init flags)
    |> U.mapModel (\m -> { m | canvasListModel = CanvasList.setContainerId "manifest_view_canvas_list" m.canvasListModel })
    |> U.chain (manifestMenu.init flags)


emptyModel : Model
emptyModel  = 
  { manifest = Nothing
  , canvas = Nothing
  , canvasListModel = CanvasList.emptyModel
  , annotation = Nothing
  , osdElemId = "manifest_view_osd"
  , menuModel = ManifestMenu.emptyModel
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifest -> 
      let
        maybeFirstCanvasUri = 
          maybeManifest
          |> Maybe.map .sequences |> Maybe.andThen List.head
          |> Maybe.map .canvases |> Maybe.andThen List.head 
          |> Maybe.map .id
      in 
        update (SetManifestAndCanvas maybeManifest maybeFirstCanvasUri) model
    SetCanvas maybeCanvasUri -> update (SetManifestAndCanvas model.manifest maybeCanvasUri) model
    SetManifestAndCanvas maybeManifest maybeCanvasUri ->
      let
        maybeManifestUri = Maybe.map .id maybeManifest
        needsLoading = maybeManifest
                        |> Maybe.map isStub
                        |> Maybe.withDefault False
        loadMsg = 
          if needsLoading then [LoadManifest (Maybe.withDefault "" (Maybe.map .id maybeManifest))]
          else []
        manifestChanging = (Maybe.map .id model.manifest) /= maybeManifestUri
      in
      ({model | manifest = maybeManifest, canvas = maybeCanvasUri}, Cmd.none, loadMsg)
        |> U.chain (canvasList.updater (CanvasList.SetManifest maybeManifest))
        |> U.chain (canvasList.updater (CanvasList.SelectCanvas maybeCanvasUri))
        |> U.chainIf manifestChanging (manifestMenu.updater (ManifestMenu.ResetMenu))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetManifest maybeManifest))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetCanvas maybeCanvasUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetMenuOpen (model.menuModel.open && not manifestChanging)))
        |> U.chain openCanvasInOsd
        |> U.addOut [RequestIiif SetOverlayInOsd]
        |> U.chain loadOtherContent
        |> U.chain (scrollCanvasLine (not manifestChanging))
    CanvasListMsg canvasListMsg -> canvasList.updater canvasListMsg model
    ManifestMenuMsg manifestMenuMsg -> manifestMenu.updater manifestMenuMsg model
    IiifNotification notification -> 
      (model, Cmd.none, [])
        |> U.chain (checkIiifNotification notification)
        |> U.chain (canvasList.updater (CanvasList.IiifNotification notification))
    CloseClicked -> (model, Cmd.none, [CloseViewer])
    SetMenuOpen open -> 
      let 
        menuModel = model.menuModel
        newMenuModel = { menuModel | open = open }
      in ({model | menuModel = newMenuModel}, Cmd.none, [])
    ShowAnnotationPort iiif maybeAnnotationUri -> 
      let 
        maybeCanvas = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing
        maybeAnnotation = Maybe.map2 (getCanvasAnnotation iiif) maybeCanvas maybeAnnotationUri |> Maybe.withDefault Nothing
      in ({model | annotation = maybeAnnotation}, Cmd.none, [])
    SetOverlayInOsd iiif -> setOverlayInOsd iiif model


checkIiifNotification : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
checkIiifNotification notification model =
  case model.manifest of
    Just modelManifest ->
      case notification of
        Iiif.Loading.ManifestLoaded iiif manifestUri -> 
          if manifestUri == modelManifest.id then
            let loadedManifest = getManifest iiif manifestUri
            in
            case model.canvas of
              Nothing -> update (SetManifest (Just loadedManifest)) model
              Just canvasUri -> update (SetManifestAndCanvas (Just loadedManifest) (Just canvasUri)) model
          else (model, Cmd.none, [])
        Iiif.Loading.AnnotationListLoaded iiif annotationListUri -> 
          (model, Cmd.none, [])
          |> U.chain (setOverlayInOsd iiif)
        _ -> (model, Cmd.none, [])
    Nothing -> (model, Cmd.none, [])


loadOtherContent : Model -> (Model, Cmd Msg, List OutMsg)
loadOtherContent model = 
  case (model.manifest, model.canvas) of
    (Just manifest, Just canvasUri) ->
      let
        outMsgs = getCanvas manifest canvasUri
                    |> Maybe.map getCanvasAnnotationLists
                    |> Maybe.withDefault []
                    |> List.map LoadAnnotationList
      in
        (model, Cmd.none, outMsgs)
    _ -> (model, Cmd.none, [])


titleView : Model -> Element Msg
titleView model =
  case model.manifest of
    Just manifest ->
      row [spacing 5, width fill]
        [ Button.secondary |> Button.content (TitleLine.iconOnly "arrow-left") |> Button.onPress CloseClicked |> Button.button
        , el [width fill] (ManifestTitle.empty |> ManifestTitle.manifest manifest |> ManifestTitle.attributes [centerX] |> ManifestTitle.manifestTitle)
        , Button.secondary |> Button.content (TitleLine.iconOnly "info") |> Button.onPress (SetMenuOpen (not model.menuModel.open)) |> Button.button 
        ]
    Nothing -> Element.none

view : Model -> Element Msg
view model = 
  let 
    menuElem =  if model.menuModel.open then el [alignRight, width (px 500), height fill] <| manifestMenu.view model
                else Element.none
  in
  column [spacing 0, width fill, height fill]
    [ titleView model
    , el 
      [ inFront <| el [alignRight, width (px 500), padding 15, height shrink] <| annotationView model.annotation
      , inFront <| menuElem
      , width fill, height fill
      ] (Lazy.lazy osdElement model.osdElemId)
    , canvasList.view model
    ]


osdElement : String -> Element Msg
osdElement elementId =
  Element.html <| Html.div 
    [ Html.Attributes.id elementId
    , Html.Attributes.class "osd_container"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "100%" 
    , Html.Attributes.style "position" "absolute"
    ] []