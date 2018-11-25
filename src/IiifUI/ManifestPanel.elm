module IiifUI.ManifestPanel exposing(..)

import Json.Decode as Decode
import Dict

import Html
import Html.Attributes

import Element exposing(..)
import Element.Events as Events
import Element.Input as Input
import Element.Font as Font

import UI.Panel as Panel
import UI.DefinitionList as DefinitionList
import UI.Collapsible as Collapsible
import UI.Colors as Colors
import UI.Button as Button

import IiifUI.CanvasButton as CanvasButton
import IiifUI.IiifLink as IiifLink
import IiifUI.ManifestTitle as ManifestTitle
import IiifUI.ManifestDetails as ManifestDetails
import IiifUI.Spinner as Spinner

import Update as U

import Utils exposing(pluralise)

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, isStub)

type alias Model =
  { manifest : Maybe ManifestUri
  , iiif : Iiif
  , collapsible : Collapsible.Model
  }

type Msg  = CollapsibleMsg Collapsible.Msg
          | CanvasClicked CanvasUri
          | TitleClicked

type OutMsg = ManifestSelected
            | CanvasSelected CanvasUri

component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }

collapsible = 
  U.subComponent 
    { component = Collapsible.component 
    , unwrapModel = .collapsible
    , wrapModel = \model subModel -> { model | collapsible = subModel }
    , wrapMsg = CollapsibleMsg
    , outEvaluator = \msgSub model -> (model, Cmd.none, [])
    }

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

emptyModel : Model
emptyModel = 
  { manifest = Nothing
  , iiif = Iiif.Utils.empty
  , collapsible = Collapsible.emptyModel |> Collapsible.closed |> Collapsible.labels (text "Show manifest details") (text "Hide manifest details")
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map CollapsibleMsg (Collapsible.subscriptions model.collapsible)

update : Msg -> Model -> (Model, Cmd Msg, List OutMsg)
update msg model = 
  case msg of 
    CollapsibleMsg collapsibleMsg -> 
      (model, Cmd.none, []) |> U.chain (collapsible.updater collapsibleMsg)
    CanvasClicked canvasUri -> (model, Cmd.none, [CanvasSelected canvasUri])
    TitleClicked -> (model, Cmd.none, [ManifestSelected])

manifest : ManifestUri -> Model -> Model
manifest manifest_ model = 
  { model |  manifest = Just manifest_ }

maybeManifest : Maybe ManifestUri -> Model -> Model
maybeManifest manifest_ model =
  { model | manifest = manifest_ }

id : String -> Model -> Model
id id_ model =
 {model | collapsible = model.collapsible |> Collapsible.id id_}

iiif : Iiif -> Model -> Model
iiif iiif_ model =
  {model | iiif = iiif_}

view : Model -> Element Msg
view model =
  case model.manifest of
    Nothing -> Element.none
    Just manifestUri -> 
      let
        manifest_ = getManifest model.iiif manifestUri
        manifestInfo = ManifestDetails.empty |> ManifestDetails.manifest manifest_ |> ManifestDetails.manifestDetails
        wrappedInfo = Panel.panelSection Panel.empty 1 (Panel.PaddedContent manifestInfo)
        lazyLoadAttrs = if isStub manifest_ then 
                            [ htmlAttribute <| Html.Attributes.class "lazyload manifest_lazyload"
                            , htmlAttribute <| Html.Attributes.attribute "data-manifest-uri" manifestUri
                            ]
                        else []        
      in
      Panel.default 
        |> Panel.attributes lazyLoadAttrs
        |> Panel.header (Input.button [Font.color Colors.link] {onPress = Just TitleClicked, label = (ManifestTitle.simple manifest_)})
        |> Panel.addSection (canvasLine model)
        |> Panel.addDirectSection (model.collapsible |> Collapsible.content wrappedInfo |> Collapsible.view |> Element.map CollapsibleMsg)
        |> Panel.footer (footer model)
        |> Panel.panel 


canvasLine : Model -> Element Msg
canvasLine model =
  case model.manifest of
    Nothing -> Element.none
    Just manifestUri -> 
      let
        manifest_ = getManifest model.iiif manifestUri
        canvases = List.head manifest_.sequences |> Maybe.map .canvases |> Maybe.withDefault [] |> List.take 10
        canvasElement canvas = 
          CanvasButton.empty 
            |> CanvasButton.canvas canvas
            |> CanvasButton.onPress (CanvasClicked canvas.id)
            |> CanvasButton.canvasButton
      in
        if isStub manifest_ then row [spacing 5] [Spinner.spinnerThumbnail]
        else row [spacing 5, clip, width fill] (List.map canvasElement canvases)


footer : Model -> Element Msg
footer model =
  case model.manifest of
    Nothing -> Element.none
    Just manifestUri -> 
      let
        manifest_ = getManifest model.iiif manifestUri
        canvases = List.head manifest_.sequences |> Maybe.map .canvases |> Maybe.withDefault []
        (collapsibleMsg, toggleLabel) = Collapsible.toggleButtonInfo model.collapsible
        toggleButton = 
          Button.slimLink 
          |> Button.color Colors.defaultTextColor 
          |> Button.content (toggleLabel |> Element.map CollapsibleMsg)
          |> Button.onPress (CollapsibleMsg collapsibleMsg)
          |> Button.button
      in
      row [spacing 5, width fill, Font.color Colors.dimTextColor]
        [ el [alignLeft] (toggleButton)
        , el [alignRight] (text <| pluralise (List.length canvases) "image -" "images -")
        , el [alignRight] (IiifLink.iiifLink manifest_.id)
        ]

