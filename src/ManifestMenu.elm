port module ManifestMenu exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

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
import Utils exposing(iiifLink, pluralise)

import ManifestDetails exposing(..)
import StructuresView

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest)
import Iiif.Loading

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
          | ResetMenu
          | TabMsg Tab.State
          | StructuresViewMsg StructuresView.Msg
          | IiifNotification Iiif.Loading.Notification

type OutMsg = RangeSelected RangeUri

structuresView =
  U.subComponent 
    { component = StructuresView.component 
    , unwrapModel = \model -> let subModel = model.structuresViewModel in {subModel | iiif = model.iiif}
    , wrapModel = \model subModel -> { model | structuresViewModel = { subModel | iiif = Iiif.Utils.empty }, errors = model.errors ++ subModel.errors}
    , wrapMsg = StructuresViewMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          StructuresView.RangeSelected rangeUri -> (model, Cmd.none, [RangeSelected rangeUri])
    }


component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view }


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  (emptyModel, Cmd.none, [])
  |> U.chain (structuresView.init flags)


emptyModel : Model
emptyModel  = 
  { iiif = Iiif.Utils.empty
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
    ResetMenu -> 
      ({model | open = False, tabState = Tab.customInitialState "manifest_view_menu_info"}, Cmd.none, [])
    SetManifest maybeManifestUri -> 
      ({model | manifest = maybeManifestUri, canvas = Nothing}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetManifest maybeManifestUri))
    SetCanvas maybeCanvasUri -> 
      ({model | canvas = maybeCanvasUri}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetCanvas maybeCanvasUri))
    SetMenuOpen open -> ({model | open = open}, Cmd.none, [])
    TabMsg state -> ({model | tabState = state}, Cmd.none, [])
    StructuresViewMsg structuresViewMsg -> structuresView.updater structuresViewMsg model
    IiifNotification notification -> (model, Cmd.none, [])


view : Model -> Html Msg
view model = 
  let
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    showClass = if model.open then " show" else ""
    maybeInfo = Maybe.map (\m -> manifestDetailsExtras m [("IIIF", Just (iiifLink m.id))]) maybeManifest 
    info = Maybe.withDefault (text "") maybeInfo
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
            , pane = Tab.pane [class "container-fluid"] [ structuresView.view model ] 
            }
        , Tab.item
            { id = "manifest_view_menu_layers" 
            , link = Tab.link [] [ text "Layers" ]
            , pane = Tab.pane [] [ text "layers tab"]
            }
        ]
      |> Tab.view model.tabState
    ]
