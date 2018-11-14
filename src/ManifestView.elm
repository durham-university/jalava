port module ManifestView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Lazy as Lazy
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button

import Update as U
import Utils exposing(iiifLink, pluralise, flip)

import CanvasList
import ManifestMenu

import Iiif exposing(..)

port osdCmd : Encode.Value -> Cmd msg

port scrollToView : Encode.Value -> Cmd msg

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvas : Maybe CanvasUri
  , canvasListModel : CanvasList.Model
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
          | IiifNotification Iiif.Notification
          | SetMenuOpen Bool
          | CloseClicked

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | CanvasOpened ManifestUri CanvasUri
            | CloseViewer


canvasList =
  U.subComponent 
    { component = CanvasList.component 
    , unwrapModel = \model -> let subModel = model.canvasListModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | canvasListModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
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
    , wrapModel = \model subModel -> { model | menuModel = { subModel | iiif = Iiif.empty }, errors = model.errors ++ subModel.errors}
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


openCanvasInOsd : Model -> ManifestUri -> CanvasUri -> (Model, Cmd Msg, List OutMsg)
openCanvasInOsd model manifestUri canvasUri = 
  let
    manifest = getManifest model.iiif manifestUri
    maybeCanvas = getCanvas manifest canvasUri
    maybeSource = Maybe.andThen osdSource maybeCanvas
    source = Maybe.withDefault "" maybeSource
    cmd = osdCmd (Encode.object 
        [ ("type", Encode.string "setSource")
        , ("for", Encode.string model.osdElemId)
        , ("value", Encode.string source)
        ])
  in
    (model, cmd, [CanvasOpened manifestUri canvasUri])


scrollCanvasLine : Bool -> Model -> (Model, Cmd Msg, List OutMsg)
scrollCanvasLine animate model =
  case model.canvas of
    Nothing -> (model, Cmd.none, [])
    Just canvasUri -> 
      let 
        buttonId = CanvasList.buttonIdFor canvasUri
        scrollCmd = scrollToView (Encode.object
          [ ("container", Encode.string ".manifest_view .canvas_list")
          , ("item", Encode.string ("#" ++ buttonId))
          , ("axis", Encode.string "x")
          , ("animate", Encode.bool animate)
          ])
      in (model, scrollCmd, [])


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    (emptyModel, Cmd.none, [])
    |> U.chain (canvasList.init flags)
    |> U.chain (manifestMenu.init flags)


emptyModel : Model
emptyModel  = 
  { iiif = Iiif.empty
  , manifest = Nothing
  , canvas = Nothing
  , canvasListModel = CanvasList.emptyModel
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
        |> U.chain (manifestMenu.updater (ManifestMenu.SetManifest maybeManifestUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetCanvas maybeCanvasUri))
        |> U.chain (manifestMenu.updater (ManifestMenu.SetMenuOpen (model.menuModel.open && not manifestChanging)))
        |> U.maybeChain (\(ma, ca) m -> openCanvasInOsd m ma ca) (Maybe.map2 (\x y -> (x, y)) maybeManifestUri maybeCanvasUri)
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


osdNotification : Iiif.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
osdNotification notification model =
  case model.manifest of
    Just modelManifestUri ->
      case notification of
        Iiif.ManifestLoaded manifestUri -> 
          if manifestUri == modelManifestUri then
            case model.canvas of
              Nothing -> update (SetManifest (Just modelManifestUri)) model
              Just canvasUri -> update (SetManifestAndCanvas (Just modelManifestUri) (Just canvasUri)) model
          else (model, Cmd.none, [])
        Iiif.CollectionLoaded collectionUri -> (model, Cmd.none, [])
    Nothing -> (model, Cmd.none, [])


view : Model -> Html Msg
view model = 
  let
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    logoHtml = case Maybe.andThen .logo maybeManifest of
        Just logo -> img [src logo, class "logo"] []
        Nothing -> text ""
    title = Maybe.withDefault "" (Maybe.map manifestToString maybeManifest)
    menuOpenClass = if model.menuModel.open then " show" else ""
  in
  div [ class "manifest_view" ] 
    [ div [ class "manifest_zoomer" ] 
      [ Lazy.lazy osdElement model.osdElemId
      ]
    , manifestMenu.view model
    , div [ class "title" ] 
      [ Button.button [Button.light, Button.attrs [ class "close_button", onClick CloseClicked ]] [i [ class "fas fa-arrow-left" ] []]
      , h1 [] [ logoHtml, text title ] 
      , Button.button [Button.light, Button.attrs [ class "menu_button", onClick (SetMenuOpen (not model.menuModel.open)) ]] [i [class "fas fa-info"] []]
      ]
    , canvasList.view model
    ]

osdElement : String -> Html Msg
osdElement elementId =
  div [ id elementId, class "osd_container" ] []