module ManifestMenu exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel, component)

import Url
import Json.Decode as Decode
import Json.Encode as Encode

import UI.Core exposing(..)

import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy exposing(lazy)

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
  { manifest : Maybe Manifest
  , canvas : Maybe CanvasUri
  , open : Bool
  , tabState : OpenTab
  , structuresViewModel : StructuresView.Model
  }

type OpenTab = TabInfo | TabStructures | TabLayers

type Msg  = SetManifest (Maybe Manifest)
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
    , unwrapModel = .structuresViewModel
    , wrapModel = \model subModel -> { model | structuresViewModel = subModel }
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
  { manifest = Nothing
  , canvas = Nothing
  , open = False
  , tabState = TabInfo
  , structuresViewModel = StructuresView.emptyModel
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    ResetMenu -> 
      ({model | open = False, tabState = TabInfo}, Cmd.none, [])
    SetManifest maybeManifest -> 
      ({model | manifest = maybeManifest, canvas = Nothing}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetManifest maybeManifest))
    SetCanvas maybeCanvasUri -> 
      ({model | canvas = maybeCanvasUri}, Cmd.none, [])
      |> U.chain (structuresView.updater (StructuresView.SetCanvas maybeCanvasUri))
    SetMenuOpen open -> ({model | open = open}, Cmd.none, [])
    SelectTab state -> ({model | tabState = state}, Cmd.none, [])
    StructuresViewMsg structuresViewMsg -> structuresView.updater structuresViewMsg model
    IiifNotification notification -> 
      case notification of 
        Iiif.Loading.ManifestLoaded iiif manifestUri -> 
          if Just manifestUri == Maybe.map .id model.manifest then
            ({model | manifest = Just <| getManifest iiif manifestUri}, Cmd.none, [])
          else (model, Cmd.none, [])
        _ -> (model, Cmd.none, [])


view : Model -> Html Msg
view model = lazy view_ model

view_ : Model -> Html Msg
view_ model = 
  if model.open then viewOpen model
  else none

viewOpen : Model -> Html Msg
viewOpen model = 
  let
    showClass = if model.open then " show" else ""
    maybeManifest = model.manifest
    tabContent = case model.tabState of
      TabInfo -> 
        case maybeManifest of
          Just manifest -> ManifestDetails.empty 
                            |> ManifestDetails.manifest manifest
                            |> ManifestDetails.includeIiifLink
                            |> ManifestDetails.manifestDetails
                            |> el [cssPadding <| cssPx 5, fullWidth, fullHeight] 
          Nothing -> none
      TabStructures -> structuresView.view model
      TabLayers -> text "Layers tab"
  in
  column 0 
    [ fullWidth
    , fullHeight
    , cssBackgroundColor <| Colors.toCss Colors.defaultBackground
    ]
    [ Tabs.default
        |> Tabs.content 
            [ (TabInfo, text "Info")
            , (TabStructures, text "Structures")
--            , (TabLayers, text "Layers")
            ]
        |> Tabs.selected model.tabState
        |> Tabs.onPress SelectTab
        |> Tabs.tabs
    , el 
      [ fullWidth
      , fullHeight
      , Attributes.style "overflow-y" "scroll"
      , Attributes.style "overflow-x" "hidden"
      , Attributes.style "flex-shrink" "1"
      , Attributes.style "word-break" "break-word"
      , Attributes.style "border-left" <| "1px solid " ++ (Colors.toCss Colors.divider)
      , Attributes.style "border-bottom" <| "1px solid " ++ (Colors.toCss Colors.divider)
      , Attributes.style "height" "100%" -- This fixes a layout bug in some browsers
      ] tabContent
    ]
