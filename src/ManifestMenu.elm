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

import Iiif exposing(..)

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , open : Bool
  , tabState : Tab.State
  , errors : List String
  }

type Msg  = SetManifest (Maybe ManifestUri)
          | SetMenuOpen Bool
          | TabMsg Tab.State
          | IiifNotification Iiif.Notification

type OutMsg = Nop



init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])


emptyModel : Model
emptyModel  = 
  { iiif = Iiif.empty
  , manifest = Nothing
  , open = False
  , tabState = Tab.initialState
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    SetManifest maybeManifestUri -> ({model | manifest = maybeManifestUri}, Cmd.none, [])
    SetMenuOpen open -> ({model | open = open}, Cmd.none, [])
    TabMsg state -> ({model | tabState = state}, Cmd.none, [])
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
            , pane = Tab.pane [class "container-fluid"] [ Grid.row [] [ Grid.col [] [info] ] ]
            }
        , Tab.item
            { id = "manifest_view_menu_toc" 
            , link = Tab.link [] [ text "ToC" ]
            , pane = Tab.pane [] [ text "toc tab"]
            }
        , Tab.item
            { id = "manifest_view_menu_layers" 
            , link = Tab.link [] [ text "Layers" ]
            , pane = Tab.pane [] [ text "layers tab"]
            }
        ]
      |> Tab.view model.tabState
    ]
