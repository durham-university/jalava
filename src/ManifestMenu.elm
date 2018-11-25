port module ManifestMenu exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html.Attributes

import Element exposing(..)
import Element.Background as Background
import UI.Tabs as Tabs
import UI.Colors as Colors
import IiifUI.IiifLink as IiifLink
import IiifUI.ManifestDetails as ManifestDetails

import Update as U
import Utils exposing(pluralise)

import StructuresView

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest)
import Iiif.Loading

type alias Model =
  { iiif : Iiif
  , manifest : Maybe ManifestUri
  , canvas : Maybe CanvasUri
  , open : Bool
  , tabState : OpenTab
  , structuresViewModel : StructuresView.Model
  , errors : List String
  }

type OpenTab = TabInfo | TabToc | TabLayers

type Msg  = SetManifest (Maybe ManifestUri)
          | SetCanvas (Maybe CanvasUri)
          | SetMenuOpen Bool
          | ResetMenu
          | SelectTab OpenTab
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
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }


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
  , tabState = TabInfo
  , structuresViewModel = StructuresView.emptyModel
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    ResetMenu -> 
      ({model | open = False, tabState = TabInfo}, Cmd.none, [])
    SetManifest maybeManifestUri -> 
      ({model | manifest = maybeManifestUri, canvas = Nothing}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetManifest maybeManifestUri))
    SetCanvas maybeCanvasUri -> 
      ({model | canvas = maybeCanvasUri}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetCanvas maybeCanvasUri))
    SetMenuOpen open -> ({model | open = open}, Cmd.none, [])
    SelectTab state -> ({model | tabState = state}, Cmd.none, [])
    StructuresViewMsg structuresViewMsg -> structuresView.updater structuresViewMsg model
    IiifNotification notification -> (model, Cmd.none, [])

view : Model -> Element.Element Msg
view model = 
  if model.open then viewOpen model
  else Element.none

viewOpen : Model -> Element.Element Msg
viewOpen model = 
  let
    showClass = if model.open then " show" else ""
    maybeManifest = Maybe.map (getManifest model.iiif) model.manifest
    tabContent = case model.tabState of
      TabInfo -> 
        case maybeManifest of
          Just manifest -> ManifestDetails.empty 
                            |> ManifestDetails.manifest manifest
                            |> ManifestDetails.includeIiifLink
                            |> ManifestDetails.manifestDetails
                            |> Element.el [padding 5, width fill] 
          Nothing -> Element.none
      TabToc -> structuresView.view model
      TabLayers -> Element.text "Layers tab"
  in
  Element.column [spacing 5, width fill, height fill, Background.color Colors.defaultBackground]
    [ Tabs.default
        |> Tabs.content 
            [ (TabInfo, Element.text "Info")
            , (TabToc, Element.text "ToC")
            , (TabLayers, Element.text "Layers")
            ]
        |> Tabs.selected model.tabState
        |> Tabs.onPress SelectTab
        |> Tabs.tabs
    , Element.el [height fill] tabContent
    ]
  
{-  
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
-}