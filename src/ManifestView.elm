port module ManifestView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode

import Html exposing(..)
import Html.Attributes as Attributes exposing(style)
import Html.Events as Events
import Html.Lazy exposing(lazy)

import UI.Core exposing(..)
import UI.Button as Button
import UI.TitleLine as TitleLine
import UI.Icon as Icon
import UI.Colors as Colors
import IiifUI.Spinner as Spinner
import IiifUI.ManifestTitle as ManifestTitle

import Update as U
import Utils exposing(pluralise, flip, ScrollInfo, ScrollAlignment(..), ScrollAxis(..))

import CanvasList
import ManifestMenu
import AnnotationView exposing(..)
import SharingTools

import Iiif.Types exposing(..)
import Iiif.Encoders
import Iiif.Loading
import Iiif.Utils exposing(getManifest, getCanvas, getRange, isStub, manifestToString, getCanvasAnnotationLists, getAnnotationLists, getCanvasAnnotation)
import Iiif.ImageApi exposing(osdSource, OsdSource(..))

port outPortOsdCmd : Encode.Value -> Cmd msg

port outPortSetAnnotationsCmd : Encode.Value -> Cmd msg

type alias Model =
  { manifest : Maybe Manifest
  , canvas : Maybe CanvasUri
  , canvasListModel : CanvasList.Model
  , annotation : Maybe Annotation
  , osdElemId : String
  , menuModel : ManifestMenu.Model
  , showSharingTools : Bool
  , sharingToolsOpen : Bool
  , selectionModel : SharingTools.Model
  }


type Msg  = SetManifest (Maybe Manifest)
          | SetCanvas (Maybe CanvasUri)
          | SetManifestAndCanvas (Maybe Manifest) (Maybe CanvasUri)
          | CanvasListMsg CanvasList.Msg
          | ManifestMenuMsg ManifestMenu.Msg
          | IiifNotification Iiif.Loading.Notification
          | SetMenuOpen Bool
          | SetSharingToolsOpen Bool
          | SharingToolsMsg SharingTools.Msg
          | SetSelection Decode.Value -- This message comes from Main, originally from a port. Converted to SelectionMsg and passed on to Selection
          | CloseClicked
          | ShowAnnotationPort Iiif (Maybe AnnotationUri)
          | SetOverlayInOsd Iiif

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | LoadAnnotationList AnnotationListUri
            | CanvasOpened ManifestUri CanvasUri
            | CloseViewer
            | RequestIiif (Iiif -> Msg)
            | ScrollToView ScrollInfo
            | CopyToClipboard String


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
          CanvasList.ScrollToViewOut scrollInfo -> 
            (model, Cmd.none, [ScrollToView scrollInfo])
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

sharingTools =
  U.subComponent
    { component = SharingTools.component
    , unwrapModel = .selectionModel
    , wrapModel = \model subModel -> {model | selectionModel = subModel }
    , wrapMsg = SharingToolsMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          SharingTools.CopyToClipboard s -> (model, Cmd.none, [CopyToClipboard s])
    }

openCanvasInOsd : Model -> (Model, Cmd Msg, List OutMsg)
openCanvasInOsd model = 
  let
    maybeCanvas = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing
    maybeSource = Maybe.andThen osdSource maybeCanvas
    maybeSourceValue = 
      case maybeSource of
        Nothing -> Nothing
        Just (ImageSource url) -> Just <| Encode.object [("sourceType", Encode.string "image"), ("url", Encode.string url)]
        Just (IiifSource url) -> Just <| Encode.object [("sourceType", Encode.string "iiif"), ("url", Encode.string url)]
    maybeCmd = Maybe.map
      (\sourceValue -> 
        outPortOsdCmd (Encode.object 
          [ ("type", Encode.string "setSource")
          , ("for", Encode.string model.osdElemId)
          , ("value", sourceValue)
          ])
      ) maybeSourceValue
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
  let
    decodedShowSharingTools = Decode.decodeValue (Decode.field "sharingTools" Decode.bool) flags

    setShowSharingTools = 
      case decodedShowSharingTools of
        Result.Ok u -> U.mapModel (\m -> {m | showSharingTools = u})
        Result.Err e -> identity
  in
  (emptyModel, Cmd.none, [])
    |> U.chain (canvasList.init flags)
    |> U.mapModel (\m -> { m | canvasListModel = CanvasList.setContainerId "manifest_view_canvas_list" m.canvasListModel })
    |> U.chain (manifestMenu.init flags)
    |> U.chain (sharingTools.init flags)
    |> setShowSharingTools


emptyModel : Model
emptyModel  = 
  { manifest = Nothing
  , canvas = Nothing
  , canvasListModel = CanvasList.emptyModel
  , annotation = Nothing
  , osdElemId = "manifest_view_osd"
  , menuModel = ManifestMenu.emptyModel
  , showSharingTools = True
  , sharingToolsOpen = False
  , selectionModel = SharingTools.emptyModel
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
        maybeCanvas = case (maybeManifest, maybeCanvasUri) of
                        (Just m, Just c) -> getCanvas m c
                        (_, _) -> Nothing
      in
      ({model | manifest = maybeManifest, canvas = maybeCanvasUri}, Cmd.none, loadMsg)
        |> U.chain (canvasList.updater (CanvasList.SetManifest maybeManifest))
        |> U.chain (canvasList.updater (CanvasList.SelectCanvas maybeCanvasUri))
        |> U.chainIf manifestChanging (manifestMenu.updater (ManifestMenu.ResetMenu))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetManifest maybeManifest))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetCanvas maybeCanvasUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetMenuOpen (model.menuModel.open && not manifestChanging)))
        |> U.chain (sharingTools.updater (SharingTools.SetCanvas (Maybe.map2 (\a b -> (a, b)) maybeManifest maybeCanvasUri)))
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
        toolsOpen = if open then False else model.sharingToolsOpen
      in 
      ({model | menuModel = newMenuModel}, Cmd.none, [])
      |> U.chain (setToolsOpen toolsOpen)
    SetSharingToolsOpen open ->
      let
        menuModel = model.menuModel
        newMenuModel = if open then { menuModel | open = False } else menuModel
      in 
      ({model | menuModel = newMenuModel}, Cmd.none, [])
      |> U.chain (setToolsOpen open)
    SetSelection value -> sharingTools.updater (SharingTools.SetSelection value) model
    SharingToolsMsg selectionMsg -> sharingTools.updater selectionMsg model
    ShowAnnotationPort iiif maybeAnnotationUri -> 
      let 
        maybeCanvas = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing
        maybeAnnotation = Maybe.map2 (getCanvasAnnotation iiif) maybeCanvas maybeAnnotationUri |> Maybe.withDefault Nothing
      in ({model | annotation = maybeAnnotation}, Cmd.none, [])
    SetOverlayInOsd iiif -> setOverlayInOsd iiif model


setToolsOpen : Bool -> Model -> (Model, Cmd Msg, List OutMsg)
setToolsOpen open model = 
  ({model | sharingToolsOpen = open}, Cmd.none, [])
  |> U.chain (sharingTools.updater (SharingTools.SetSelectionEnabled open))

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


titleView : Model -> Html Msg
titleView model =
  let
    toolsButton = Button.light
                  |> Button.content (TitleLine.iconOnly "share-square")
                  |> Button.onPress (SetSharingToolsOpen (not model.sharingToolsOpen)) 
                  |> Button.attributes [style "width" <| cssPx 16]
                  |> Button.round 0
                  |> Button.button
    infoButton =  Button.light 
                  |> Button.content (TitleLine.iconOnly "info") 
                  |> Button.onPress (SetMenuOpen (not model.menuModel.open)) 
                  |> Button.attributes [style "width" <| cssPx 16] 
                  |> Button.round 0
                  |> Button.button 
    buttons = if model.showSharingTools then
                [ toolsButton, infoButton ]
              else
                [ infoButton ]
  in
  case model.manifest of
    Just manifest ->
      row 5 [fullWidth]
        [ Button.light 
            |> Button.content (TitleLine.iconOnly "arrow-left") 
            |> Button.onPress CloseClicked 
            |> Button.attributes [style "width" <| cssPx 16] 
            |> Button.round 0
            |> Button.button
        , column 0 [fullWidth, style "flex-shrink" "1"] 
            [ ManifestTitle.empty 
                |> ManifestTitle.manifest manifest 
                |> ManifestTitle.attributes [style "align-self" "center"] 
                |> ManifestTitle.manifestTitle ]
        , row 0 [] buttons
        ]
    Nothing -> none

view : Model -> Html Msg
view model = 
  let 
    menuElem =  if model.menuModel.open then 
                    el 
                      [ style "position" "absolute"
                      , style "top" "0", style "bottom" "0", style "right" "0"
                      , cssWidth <| cssPx 500, fullHeight] 
                      <| manifestMenu.view model
                else div [Attributes.class "hide"] []
    toolsElem = if model.sharingToolsOpen then
                    el 
                      [ fullHeight, cssWidth <| cssPx 400 ]
                      <| toolsView model
                else div [Attributes.class "hide"] []
    annotationElem = case model.annotation of
                        Just _ -> 
                          el 
                            [ style "position" "absolute"
                            , style "top" "0", style "right" "0"
                            , style "width" <| cssPx 500
                            , cssPadding <| cssPx 15] 
                            <| annotationView [fullWidth] model.annotation
                        Nothing -> div [Attributes.class "hide"] []
  in
  column 0 [fullWidth, fullHeight]
    [ titleView model
    , row 0 [fullWidth, fullHeight, style "flex-shrink" "1"] 
      [ el [fullWidth, fullHeight] (lazy osdElement model.osdElemId)
      , menuElem
      , toolsElem
      , annotationElem
      ]
    , canvasList.view model
    ]


toolsView : Model -> Html Msg
toolsView model =
  sharingTools.view model


osdElement : String -> Html Msg
osdElement elementId =
  Html.div 
    [ Attributes.id elementId
    , Attributes.class "osd_container"
    , style "width" "100%"
    , style "height" "100%" 
    , style "position" "absolute"
    ] []