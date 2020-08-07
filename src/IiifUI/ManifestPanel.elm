port module IiifUI.ManifestPanel exposing(..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode
import Dict
import Regex

import Iiif.Loading

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy

import UI.Core exposing(..)
import UI.Panel as Panel
import UI.DefinitionList as DefinitionList
import UI.Collapsible as Collapsible
import UI.Colors as Colors
import UI.Button as Button
import UI.Error exposing(err)

import IiifUI.CanvasButton as CanvasButton
import IiifUI.IiifLink as IiifLink
import IiifUI.ManifestTitle as ManifestTitle
import IiifUI.ManifestDetails as ManifestDetails
import IiifUI.Spinner as Spinner

import Update as U

import Utils exposing(pluralise)

import Iiif.Types exposing(..)
import Iiif.Utils exposing(getManifest, willLoad, contentState)

port outPortManifestLink : Encode.Value -> Cmd msg

type alias Model =
  { manifest : Maybe Manifest
  , collapsible : Collapsible.Model
  , linkers : List ManifestLinker
  }

type Msg  = CollapsibleMsg Collapsible.Msg
          | CanvasClicked CanvasUri
          | TitleClicked
          | IiifNotification Iiif.Loading.Notification
          | PortLinkClicked Decode.Value

type OutMsg = ManifestSelected
            | CanvasSelected CanvasUri

type ManifestLink = UrlLink { url : String
                            , icon : Maybe String
                            , label : Maybe String
                            }
                  | PortLink { param : Decode.Value
                              , icon : Maybe String
                              , label : Maybe String
                              }

type alias ManifestLinker = Manifest -> Maybe ManifestLink

type ManifestLinkMethod = ReplaceContentState String
                        | ReplaceManifestUri String
                        | PortManifestUri Decode.Value
                        | NoLink

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

makeManifestLinker : Maybe String -> Maybe String -> Maybe String -> ManifestLinkMethod -> ManifestLinker
makeManifestLinker maybeLabel maybeIcon maybeMatchManifest manifestLinkMethod =
  let
    maybeManifestRegex = Maybe.andThen Regex.fromString maybeMatchManifest

    replacer : ManifestLinker
    replacer m =
      case manifestLinkMethod of
        NoLink -> Nothing
        ReplaceContentState replaceContentState -> 
          case contentState m Nothing Nothing Nothing of
            Nothing -> Nothing
            Just contentState_ -> 
              Just <| UrlLink { url = String.replace "__contentstate__" contentState_ replaceContentState
                              , label = maybeLabel
                              , icon = maybeIcon
                              }
        ReplaceManifestUri replaceManifestUri ->
          Just <| UrlLink { url = String.replace "__manifesturi__" m.id replaceManifestUri
                          , label = maybeLabel
                          , icon = maybeIcon
                          }
        PortManifestUri param ->
          Just <| PortLink { param = Encode.object [("uri", Encode.string m.id), ("param", param)]
                            , label = maybeLabel
                            , icon = maybeIcon
                            }
    
    matcher : ManifestLinker -> ManifestLinker
    matcher linker m = 
      case maybeManifestRegex of
        Nothing -> linker m
        Just manifestRegex -> 
          if List.isEmpty <| Regex.find manifestRegex m.id then
            Nothing
          else
            linker m

  in
  matcher replacer

manifestLinkerDecoder : Decode.Decoder ManifestLinker
manifestLinkerDecoder = 
  Decode.succeed makeManifestLinker
  |> DecodeP.optional "label" (Decode.nullable Decode.string) Nothing
  |> DecodeP.optional "icon" (Decode.nullable Decode.string) Nothing
  |> DecodeP.optional "matchManifest" (Decode.nullable Decode.string) Nothing
  |> DecodeP.custom (Decode.oneOf
      [ Decode.succeed ReplaceContentState |> DecodeP.required "replaceContentState" Decode.string
      , Decode.succeed ReplaceManifestUri |> DecodeP.required "replaceManifestUri" Decode.string
      , Decode.succeed PortManifestUri |> DecodeP.required "portManifestUri" Decode.value
      , Decode.succeed NoLink
      ])

manifestLinkersDecoder : Decode.Decoder (List ManifestLinker)
manifestLinkersDecoder = Decode.list manifestLinkerDecoder


init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

emptyModel : Model
emptyModel = 
  { manifest = Nothing
  , collapsible = 
      Collapsible.emptyModel 
        |> Collapsible.closed 
        |> Collapsible.labels (text "Show manifest details") (text "Hide manifest details")
        |> Collapsible.attributes [fullWidth]
  , linkers = []
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
    IiifNotification notification -> 
      checkManifestLoaded notification model
    PortLinkClicked param -> (model, outPortManifestLink param, [])

checkManifestLoaded : Iiif.Loading.Notification -> Model -> (Model, Cmd Msg, List OutMsg)
checkManifestLoaded notification model =
  case notification of
    Iiif.Loading.ManifestLoaded iiif manifestUri ->
      if Just manifestUri == Maybe.map .id model.manifest then
        let 
          manifest_ = getManifest iiif manifestUri
        in 
          ({model | manifest = Just manifest_}, Cmd.none, []) |> U.mapModel updateManifestInfo
      else (model, Cmd.none, [])
    _ -> (model, Cmd.none, [])

updateManifestInfo : Model -> Model
updateManifestInfo model = 
  case model.manifest of
    Just manifest_ -> 
      let 
        manifestInfo = ManifestDetails.empty |> ManifestDetails.manifest manifest_ |> ManifestDetails.manifestDetails
        wrappedInfo = Panel.panelSection Panel.default 1 (Panel.PaddedContent manifestInfo)
      in 
        {model | collapsible = model.collapsible |> Collapsible.content wrappedInfo}
    Nothing -> {model | collapsible = model.collapsible |> Collapsible.content none}

manifest : Manifest -> Model -> Model
manifest manifest_ model = 
  { model |  manifest = Just manifest_ } |> updateManifestInfo

maybeManifest : Maybe Manifest -> Model -> Model
maybeManifest manifest_ model =
  { model | manifest = manifest_ }

id : String -> Model -> Model
id id_ model =
 {model | collapsible = model.collapsible |> Collapsible.id id_}

linkers : List ManifestLinker -> Model -> Model
linkers linkers_ model =
  {model | linkers = linkers_ }

view : Model -> Html Msg
view model = Lazy.lazy view_ model

view_ : Model -> Html Msg
view_ model =
  case model.manifest of
    Nothing -> none
    Just manifest_ -> 
      let
--        _ = Debug.log "Rendering" manifest_.id
        lazyLoadAttrs = if willLoad manifest_ then 
                            [ Attributes.class "lazyload manifest_lazyload"
                            , Attributes.attribute "data-manifest-uri" manifest_.id
                            ]
                        else []        
      in
      Panel.default 
        |> Panel.attributes (lazyLoadAttrs ++ [fullWidth])
        |> Panel.header (UI.Core.button 
                [ cssColor <| Colors.toCss Colors.link
                , Attributes.style "cursor" "pointer"
                , Events.onClick TitleClicked] (ManifestTitle.simple manifest_))
        |> Panel.addSection ( case Maybe.map .status model.manifest of
                                Just (Error e) -> err e
                                _ -> canvasLine model )
        |> Panel.addDirectSection (model.collapsible |> Collapsible.view |> Html.map CollapsibleMsg)
        |> Panel.footer (footer model)
        |> Panel.panel 
        |> keyedEl manifest_.id [fullWidth]


canvasLine : Model -> Html Msg
canvasLine model =
  case model.manifest of
    Nothing -> none
    Just manifest_ -> 
      let
        canvases = List.head manifest_.sequences |> Maybe.map .canvases |> Maybe.withDefault [] |> List.take 10
        canvasElement canvas = 
          CanvasButton.empty 
            |> CanvasButton.canvas canvas
            |> CanvasButton.onPress (CanvasClicked canvas.id)
            |> CanvasButton.canvasButton
      in
        if willLoad manifest_ then row 5 [Attributes.style "min-height" "60px"] [Spinner.spinnerThumbnail]
        else row 5 [Attributes.style "overflow" "hidden", Attributes.style "min-height" "60px", fullWidth] (List.map canvasElement canvases)


manifestLinkView : ManifestLink -> Html Msg
manifestLinkView link =
  let 
    content data =
      case (data.icon, data.label) of
        (Just icon, Just label) -> img [Attributes.height 18, Attributes.src icon, Attributes.alt label] []
        (Just icon, Nothing) -> img [Attributes.height 18, Attributes.src icon] []
        (Nothing, Just label) -> text label
        (Nothing, Nothing) -> text "Link"
  in
  case link of
    UrlLink data -> a [Attributes.href data.url, Attributes.target "_blank"] [Button.link |> Button.attributes [Attributes.style "padding" "0"] |> Button.content (content data) |> Button.button]
    PortLink data -> Button.link |> Button.attributes [Attributes.style "padding" "0"] |> Button.content (content data) |> Button.onPress (PortLinkClicked data.param) |> Button.button

footer : Model -> Html Msg
footer model =
  case model.manifest of
    Nothing -> none
    Just manifest_ -> 
      let
        canvases = List.head manifest_.sequences |> Maybe.map .canvases |> Maybe.withDefault []
        (collapsibleMsg, toggleLabel) = Collapsible.toggleButtonInfo model.collapsible
        toggleButton = 
          Button.slimLink 
          |> Button.color "Dim"
          |> Button.content (toggleLabel |> Html.map CollapsibleMsg)
          |> Button.onPress (CollapsibleMsg collapsibleMsg)
          |> Button.button

        manifestLinks = List.filterMap (\linker -> 
            case linker manifest_ of
              Just link -> Just <| el [] (manifestLinkView link)
              Nothing -> Nothing) model.linkers
      in
      row 5 [fullWidth, cssColor <| Colors.toCss Colors.dimTextColor] <| 
        [ el [fullWidth, Attributes.style "flex-shrink" "1"] (toggleButton)
        , el [] (text <| pluralise (List.length canvases) "image -" "images -")
        , el [] (IiifLink.iiifLink manifest_.id)
        ] ++ manifestLinks

