port module ManifestView exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

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
import Config
import Utils exposing(iiifLink, pluralise)

import CanvasList
import ManifestMenu

import Iiif exposing(..)

port osdCmd : Encode.Value -> Cmd msg

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


canvasListSubModel : Model -> CanvasList.Model
canvasListSubModel model =
  let subModel = model.canvasListModel
  in { subModel | iiif = model.iiif }

canvasListOutEvaluator : CanvasList.OutMsg -> Model -> (Model, Cmd Msg, List OutMsg)
canvasListOutEvaluator msg model =
  case msg of
    CanvasList.CanvasOpened uri -> 
      case model.manifest of
        Nothing -> (model, Cmd.none, [])
        Just manifestUri -> 
          openCanvasInOsd {model | canvas = Just uri} manifestUri uri

canvasListUpdater : CanvasList.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
canvasListUpdater msg model =
  CanvasList.update msg (canvasListSubModel model)
    |> canvasListPipe model

canvasListPipe : Model -> (CanvasList.Model, Cmd CanvasList.Msg, List CanvasList.OutMsg) -> (Model, Cmd Msg, List OutMsg)
canvasListPipe model = 
  U.mapCmd CanvasListMsg
  >> U.mapModel (\m -> { model | canvasListModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.evalOut canvasListOutEvaluator


manifestMenuSubModel : Model -> ManifestMenu.Model
manifestMenuSubModel model =
  let subModel = model.menuModel
  in { subModel | iiif = model.iiif }

manifestMenuOutMapper : ManifestMenu.OutMsg -> List OutMsg
manifestMenuOutMapper msg =
  case msg of
    ManifestMenu.Nop -> []

manifestMenuUpdater : ManifestMenu.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
manifestMenuUpdater msg model =
  ManifestMenu.update msg (manifestMenuSubModel model)
    |> manifestMenuPipe model

manifestMenuPipe : Model -> (ManifestMenu.Model, Cmd ManifestMenu.Msg, List ManifestMenu.OutMsg) -> (Model, Cmd Msg, List OutMsg)
manifestMenuPipe model = 
  U.mapCmd ManifestMenuMsg
  >> U.mapModel (\m -> { model | menuModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut manifestMenuOutMapper



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


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let 
    baseModel = emptyModel 
  in
    CanvasList.init flags
      |> canvasListPipe baseModel
--      |> U.chain (openUrl url)


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
    SetManifestAndCanvas maybeManifestUri maybeCanvasUri ->
      let
        needsLoading = Maybe.map (getManifest model.iiif) maybeManifestUri 
                        |> Maybe.map isStub
                        |> Maybe.withDefault False
        loadMsg = 
          if needsLoading then [LoadManifest (Maybe.withDefault "" maybeManifestUri)]
          else []
      in
      ({model | manifest = maybeManifestUri, canvas = maybeCanvasUri}, Cmd.none, loadMsg)
        |> U.chain (canvasListUpdater (CanvasList.SetManifest maybeManifestUri))
        |> U.chain (canvasListUpdater (CanvasList.SelectCanvas maybeCanvasUri))
        |> U.chain (manifestMenuUpdater (ManifestMenu.SetManifest maybeManifestUri))
        |> U.maybeChain (\(ma, ca) m -> openCanvasInOsd m ma ca) (Maybe.map2 (\x y -> (x, y)) maybeManifestUri maybeCanvasUri)
    CanvasListMsg canvasListMsg -> canvasListUpdater canvasListMsg model
    ManifestMenuMsg manifestMenuMsg -> manifestMenuUpdater manifestMenuMsg model
    IiifNotification notification -> 
      (model, Cmd.none, [])
        |> U.chain (osdNotification notification)
        |> U.chain (canvasListUpdater (CanvasList.IiifNotification notification))
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
    canvasList = Html.map CanvasListMsg <| CanvasList.view (canvasListSubModel model)
    manifestMenu = Html.map ManifestMenuMsg <| ManifestMenu.view (manifestMenuSubModel model)
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
    , manifestMenu
    , div [ class "title" ] 
      [ Button.button [Button.light, Button.attrs [ class "close_button", onClick CloseClicked ]] [i [ class "fas fa-arrow-left" ] []]
      , h1 [] [ logoHtml, text title ] 
      , Button.button [Button.light, Button.attrs [ class "menu_button", onClick (SetMenuOpen (not model.menuModel.open)) ]] [i [class "fas fa-bars"] []]
      ]
    , canvasList
    ]

osdElement : String -> Html Msg
osdElement elementId =
  div [ id elementId, class "osd_container" ] []