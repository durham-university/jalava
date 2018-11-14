port module ManifestMenu exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Tab as Tab

import Update as U
import Config
import Utils exposing(iiifLink, pluralise)

import ManifestDetails exposing(..)
import StructuresView

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvas : Maybe CanvasUri
  , open : Bool
  , tabState : Tab.State
  , structuresViewModel : StructuresView.Model
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | SetCanvas (Maybe CanvasUri)
          | SetMenuOpen Bool
          | TabMsg Tab.State
          | StructuresViewMsg StructuresView.Msg
          | IiifNotification Iiif.Notification

type OutMsg = RangeSelected RangeUri


structuresViewSubModel : Model -> StructuresView.Model
structuresViewSubModel model =
  let subModel = model.structuresViewModel
  in { subModel | iiif = model.iiif }

structuresViewOutMapper : StructuresView.OutMsg -> List OutMsg
structuresViewOutMapper msg =
  case msg of
    StructuresView.RangeSelected rangeUri -> [RangeSelected rangeUri]

structuresViewUpdater : StructuresView.Msg -> Model -> (Model, Cmd Msg, List OutMsg)
structuresViewUpdater msg model =
  StructuresView.update msg (structuresViewSubModel model)
    |> structuresViewPipe model

structuresViewPipe : Model -> (StructuresView.Model, Cmd StructuresView.Msg, List StructuresView.OutMsg) -> (Model, Cmd Msg, List OutMsg)
structuresViewPipe model =
  U.mapCmd StructuresViewMsg
  >> U.mapModel (\m -> { model | structuresViewModel = {m | iiif = Iiif.empty}, errors = model.errors ++ m.errors})
  >> U.mapOut structuresViewOutMapper



init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])


emptyModel : Model
emptyModel  = 
  { iiif = Iiif.empty
  , manifest = Nothing
  , canvas = Nothing
  , open = False
  , tabState = Tab.initialState
  , structuresViewModel = StructuresView.emptyModel
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> 
      ({model | manifest = maybeManifestUri, canvas = Nothing}, Cmd.none, [])
      |> U.chain (structuresViewUpdater (StructuresView.SetManifest maybeManifestUri))
    SetCanvas maybeCanvasUri -> 
      ({model | canvas = maybeCanvasUri}, Cmd.none, [])
      |> U.chain (structuresViewUpdater (StructuresView.SetCanvas maybeCanvasUri))
    SetMenuOpen open -> ({model | open = open}, Cmd.none, [])
    TabMsg state -> ({model | tabState = state}, Cmd.none, [])
    StructuresViewMsg structuresViewMsg -> structuresViewUpdater structuresViewMsg model
    IiifNotification notification -> (model, Cmd.none, [])


view : Model -> Html Msg
view model = 
  let
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    showClass = if model.open then " show" else ""
    maybeInfo = Maybe.map (\m -> manifestDetailsExtras m [("IIIF", Just (iiifLink m.id))]) maybeManifest 
    info = Maybe.withDefault (text "") maybeInfo
    structures = Html.map StructuresViewMsg <| StructuresView.view (structuresViewSubModel model)
  in
  div [ class ("manifest_view_menu" ++ showClass) ]
    [ Tab.config TabMsg
      |> Tab.items
        [ Tab.item 
            { id = "manifest_view_menu_info" 
            , link = Tab.link [] [ text "Info" ]
            , pane = Tab.pane [class "container-fluid"] [ info ]
            }
        , Tab.item
            { id = "manifest_view_menu_toc" 
            , link = Tab.link [] [ text "ToC" ]
            , pane = Tab.pane [class "container-fluid"] [ structures ] 
            }
        , Tab.item
            { id = "manifest_view_menu_layers" 
            , link = Tab.link [] [ text "Layers" ]
            , pane = Tab.pane [] [ text "layers tab"]
            }
        ]
      |> Tab.view model.tabState
    ]
