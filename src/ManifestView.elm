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
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvas : Maybe CanvasUri
  , canvasListModel : CanvasList.Model
  , annotation : Maybe AnnotationUri
  , osdElemId : String
  , menuModel : ManifestMenu.Model
  , errors : List String
  }

type alias MenuModel =
  { open : Bool
  }


type Msg  = SetManifest (Maybe ManifestUri)
          | SetCanvas (Maybe CanvasUri)
          | SetManifestAndCanvas (Maybe ManifestUri) (Maybe CanvasUri)
          | CanvasListMsg CanvasList.Msg
          | ManifestMenuMsg ManifestMenu.Msg
          | IiifNotification Iiif.Loading.Notification
          | SetMenuOpen Bool
          | CloseClicked
          | ShowAnnotationPort (Maybe AnnotationUri)

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | LoadAnnotationList AnnotationListUri
            | CanvasOpened ManifestUri CanvasUri
            | CloseViewer


canvasList =
  U.subComponent 
    { component = CanvasList.component 
    , unwrapModel = \model -> let subModel = model.canvasListModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | canvasListModel = { subModel | iiif = Iiif.Utils.empty }, errors = model.errors ++ subModel.errors}
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
    , unwrapModel = \model -> let subModel = model.menuModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | menuModel = { subModel | iiif = Iiif.Utils.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = ManifestMenuMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          ManifestMenu.RangeSelected rangeUri ->
            let
              maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
              maybeRange = Maybe.andThen (flip getRange <| rangeUri) maybeManifest
              maybeCanvasUri = Maybe.andThen (List.head) (Maybe.map .canvases maybeRange)
            in
              case maybeCanvasUri of
                Just canvasUri -> update (SetCanvas (Just canvasUri)) model
                Nothing -> (model, Cmd.none, [])
    }


openCanvasInOsd : ManifestUri -> CanvasUri -> Model -> (Model, Cmd Msg, List OutMsg)
openCanvasInOsd manifestUri canvasUri model = 
  let
    manifest = getManifest model.iiif manifestUri
    maybeCanvas = getCanvas manifest canvasUri
    maybeSource = Maybe.andThen osdSource maybeCanvas
    source = Maybe.withDefault "" maybeSource
    cmd = outPortOsdCmd (Encode.object 
        [ ("type", Encode.string "setSource")
        , ("for", Encode.string model.osdElemId)
        , ("value", Encode.string source)
        ])
  in
    (model, cmd, [CanvasOpened manifestUri canvasUri])


setOverlayInOsd : ManifestUri -> CanvasUri -> Model -> (Model, Cmd Msg, List OutMsg)
setOverlayInOsd manifestUri canvasUri model =
  let
    manifest = getManifest model.iiif manifestUri

    maybeCanvas = getCanvas manifest canvasUri

    annotations : List Annotation
    annotations = maybeCanvas
                    |> Maybe.map getCanvasAnnotationLists
                    |> Maybe.map (getAnnotationLists model.iiif)
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
  { iiif = Iiif.Utils.empty
  , manifest = Nothing
  , canvas = Nothing
  , canvasListModel = CanvasList.emptyModel
  , annotation = Nothing
  , osdElemId = "manifest_view_osd"
  , menuModel = ManifestMenu.emptyModel
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> 
      let
        maybeFirstCanvasUri = 
          Maybe.map (getManifest model.iiif) maybeManifestUri
          |> Maybe.map .sequences |> Maybe.andThen List.head
          |> Maybe.map .canvases |> Maybe.andThen List.head 
          |> Maybe.map .id
      in 
        update (SetManifestAndCanvas maybeManifestUri maybeFirstCanvasUri) model
    SetCanvas maybeCanvasUri -> update (SetManifestAndCanvas model.manifest maybeCanvasUri) model
    SetManifestAndCanvas maybeManifestUri maybeCanvasUri ->
      let
        needsLoading = Maybe.map (getManifest model.iiif) maybeManifestUri 
                        |> Maybe.map isStub
                        |> Maybe.withDefault False
        loadMsg = 
          if needsLoading then [LoadManifest (Maybe.withDefault "" maybeManifestUri)]
          else []
        manifestChanging = model.manifest /= maybeManifestUri
      in
      ({model | manifest = maybeManifestUri, canvas = maybeCanvasUri}, Cmd.none, loadMsg)
        |> U.chain (canvasList.updater (CanvasList.SetManifest maybeManifestUri))
        |> U.chain (canvasList.updater (CanvasList.SelectCanvas maybeCanvasUri))
        |> U.chainIf manifestChanging (manifestMenu.updater (ManifestMenu.ResetMenu))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetManifest maybeManifestUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetCanvas maybeCanvasUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetMenuOpen (model.menuModel.open && not manifestChanging)))
        |> U.maybeChain2 openCanvasInOsd maybeManifestUri maybeCanvasUri
        |> U.maybeChain2 setOverlayInOsd maybeManifestUri maybeCanvasUri
        |> U.chain loadOtherContent
        |> U.chain (scrollCanvasLine (not manifestChanging))
    CanvasListMsg canvasListMsg -> canvasList.updater canvasListMsg model
    ManifestMenuMsg manifestMenuMsg -> manifestMenu.updater manifestMenuMsg model
    IiifNotification notification -> 
      (model, Cmd.none, [])
        |> U.chain (osdNotification notification)
        |> U.chain (canvasList.updater (CanvasList.IiifNotification notification))
    CloseClicked -> (model, Cmd.none, [CloseViewer])
    SetMenuOpen open -> 
      let 
        menuModel = model.menuModel
        newMenuModel = { menuModel | open = open }
      in ({model | menuModel = newMenuModel}, Cmd.none, [])
    ShowAnnotationPort maybeAnnotationUri -> ( {model | annotation = maybeAnnotationUri}, Cmd.none, [])


loadOtherContent : Model -> (Model, Cmd Msg, List OutMsg)
loadOtherContent model = 
  case (model.manifest, model.canvas) of
    (Just manifestUri, Just canvasUri) ->
      let
        manifest = getManifest model.iiif manifestUri
        outMsgs = getCanvas manifest canvasUri
                    |> Maybe.map getCanvasAnnotationLists
                    |> Maybe.withDefault []
                    |> List.map LoadAnnotationList
      in
        (model, Cmd.none, outMsgs)
    _ -> (model, Cmd.none, [])


osdNotification : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
osdNotification notification model =
  case model.manifest of
    Just modelManifestUri ->
      case notification of
        Iiif.Loading.ManifestLoaded manifestUri -> 
          if manifestUri == modelManifestUri then
            case model.canvas of
              Nothing -> update (SetManifest (Just modelManifestUri)) model
              Just canvasUri -> update (SetManifestAndCanvas (Just modelManifestUri) (Just canvasUri)) model
          else (model, Cmd.none, [])
        Iiif.Loading.CollectionLoaded collectionUri -> (model, Cmd.none, [])
        Iiif.Loading.AnnotationListLoaded annotationListUri -> 
          (model, Cmd.none, [])
          |> U.maybeChain (setOverlayInOsd modelManifestUri) model.canvas
    Nothing -> (model, Cmd.none, [])


titleView : Model -> Element Msg
titleView model =
  let
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
  in
    case maybeManifest of
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
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    maybeCanvas = case (maybeManifest, model.canvas) of
      (Just manifest, Just canvasUri) -> getCanvas manifest canvasUri
      _ -> Nothing
    maybeAnnotation = case (maybeCanvas, model.annotation) of
      (Just canvas, Just annotationUri) -> getCanvasAnnotation model.iiif canvas annotationUri
      _ -> Nothing
  in
  column [spacing 0, width fill, height fill]
    [ titleView model
    , el 
      [ inFront <| el [alignRight, width (px 500), padding 15, height shrink] <| annotationView maybeAnnotation
      , inFront <| el [alignRight, width (px 500), height fill] <| manifestMenu.view model
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