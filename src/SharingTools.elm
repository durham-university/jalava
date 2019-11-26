port module SharingTools exposing (Model, Msg(..), OutMsg(..), component, init, emptyModel, subscriptions, update, view)

import SelectionTypes exposing(..)

import Json.Decode as Decode
import Json.Encode as Encode

import Iiif.Types exposing(..)
import Iiif.ImageApi as ImageApi
import Iiif.Utils exposing(..)
import IiifUI.IiifLink exposing(iiifIcon)

import Bytes.Encode
import Base64

import Html exposing(..)
import Html.Attributes as Attributes exposing(style)
import Html.Events as Events
import Update as U
import UI.Core exposing(..)
import UI.TitleLine as TitleLine
import UI.Button as Button
import UI.Icon as Icon
import UI.Colors as Colors

type Msg  = SetSelectionEnabled Bool
          | SetSelection Decode.Value
          | SetCanvas (Maybe (Manifest, CanvasUri))
          | ClearSelection
          | SetLabel String
          | CopyToClipboardInt String

type OutMsg = CopyToClipboard String

port outPortSelectionCmd : Encode.Value -> Cmd msg

type alias Model =
  { selectionEnabled : Bool
  , selection : Maybe Rect
  , manifest : Maybe Manifest
  , canvas : Maybe CanvasUri
  , label : String
  , iiifViewerUrl : Maybe String
  }

component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = 
  let
    decodedRootUrl = Decode.decodeValue (Decode.field "iiifViewer" Decode.string) flags

    addIiifViewer = 
      case decodedRootUrl of
        Result.Ok u -> U.mapModel (\m -> {m | iiifViewerUrl = Just u})
        Result.Err e -> identity
  in
  (emptyModel, Cmd.none, [] )
  |> addIiifViewer

emptyModel : Model
emptyModel = 
  { selectionEnabled = False
  , selection = Nothing
  , manifest = Nothing
  , canvas = Nothing
  , label = "Link target"
  , iiifViewerUrl = Nothing
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


update : Msg -> Model -> (Model, Cmd Msg, List OutMsg)
update msg model = 
  case msg of 
    -- TODO: these all need to send out port messages
    SetSelectionEnabled status -> 
      let 
        cmd = SelectionTypes.SetSelectionEnabled status |> encodeSelectionOverlayMsg |> outPortSelectionCmd
      in
      ({ model | selectionEnabled = status }, cmd, [])
    SetSelection value ->  
      let
        decoded = Decode.decodeValue selectionDecoder value
      in
        case decoded of
          Ok selection -> ({ model | selection = selection}, Cmd.none, [])
          Err error -> (model, Cmd.none, [])
    ClearSelection -> 
      let 
        cmd = SelectionTypes.ClearSelection |> encodeSelectionOverlayMsg |> outPortSelectionCmd
      in
      ({model | selection = Nothing}, cmd, [])
    SetCanvas c -> 
      case c of
        Just (ma, ca) -> 
          ({model | manifest = Just ma, canvas = Just ca}, Cmd.none, [])
          |> U.chainIf (model.canvas /= Just ca) (update ClearSelection)
        Nothing -> ({model | manifest = Nothing, canvas = Nothing, selection = Nothing}, Cmd.none, [])
    SetLabel s -> ({model | label = s}, Cmd.none, [])
    CopyToClipboardInt s -> (model, Cmd.none, [CopyToClipboard s])

view : Model -> Html Msg
view model = 
  let
    canvasMaybe = Maybe.map2 getCanvas model.manifest model.canvas |> Maybe.withDefault Nothing
    croppedUrlMaybe = canvasMaybe
              |> Maybe.map .images
              |> Maybe.andThen List.head
              |> Maybe.map .resource
              |> Maybe.andThen .service      
              |> Maybe.map2 
                  (\rect -> ImageApi.imageServiceUrl 
                        (ImageApi.Box (round rect.x) (round rect.y) (round rect.w) (round rect.h)) 
                        ImageApi.FullSize ImageApi.NoRotation ImageApi.Default "jpg"
                  ) model.selection

    fullImageUrlMaybe = canvasMaybe
              |> Maybe.map .images
              |> Maybe.andThen List.head
              |> Maybe.map .resource
              |> Maybe.andThen .service      
              |> Maybe.map (ImageApi.imageServiceUrl ImageApi.FullRegion ImageApi.FullSize ImageApi.NoRotation ImageApi.Default "jpg")

    xywhMaybe = model.selection |> Maybe.map (\rect -> [rect.x, rect.y, rect.w, rect.h] |> List.map (String.fromInt << round) |> String.join ",")

    xywhTextMaybe = model.selection |> Maybe.map (\rect -> [("x", rect.x), ("y", rect.y), ("w", rect.w), ("h", rect.h)] |> List.map (\(label, v) -> label ++ " = " ++ (String.fromFloat v)) |> String.join " ")

    contentStateMaybe = case (model.manifest, canvasMaybe, xywhMaybe) of
      (Just m, Just c, Just xywh) -> Just <|
        "{\"@context\": \"http://iiif.io/api/presentation/0/context.json\", \"id\": \"" ++ c.id ++ "_xywh_" ++ xywh ++ 
        "\", \"type\": \"Annotation\", \"motivation\": [\"contentState\"], \"resource\": {\"type\": \"dctypes:Text\", " ++ 
        "\"format\":\"text/plain\", \"chars\":\"" ++ model.label ++ "\"}, \"target\": {\"id\":\"" ++ c.id ++ "#xywh=" ++ xywh ++
        "\", \"type\":\"Canvas\", \"partOf\":{\"id\": \"" ++ m.id ++ "\",\"type\":\"Manifest\"}}}"
      (Just m, Just c, _) -> Just <|
        "{\"id\":\"" ++ c.id ++ "\", \"type\":\"Canvas\", \"partOf\":{\"id\": \"" ++ m.id ++ "\",\"type\":\"Manifest\"}}"
      _ -> Nothing

    contentState64Maybe = Maybe.andThen (\cs -> Bytes.Encode.string cs |> Bytes.Encode.encode |> Base64.fromBytes) contentStateMaybe
    
    contentStateUrlMaybe = Maybe.map ((String.replace "+" "-") << (String.replace "/" "_") << (String.replace "=" "")) contentState64Maybe

    contentStateLinkMaybe = Maybe.map2 (\cs viewer -> viewer ++ "?iiif-content=" ++ cs) contentStateUrlMaybe model.iiifViewerUrl
  in
  column 10 
    [ fullWidth
    , fullHeight
    , cssPadding <| cssPx 10
    , cssBackgroundColor <| Colors.toCss Colors.defaultBackground
    ]
    [ case contentStateLinkMaybe of
      Just url -> 
          row 5 [fullWidth] 
          [ Button.light
                |> Button.content (TitleLine.iconOnly "copy")
                |> Button.onPress (CopyToClipboardInt url)
                |> Button.button
          , Html.a [Attributes.href url, Attributes.target "_blank"] [row 5 [fullWidth] [iiifIcon, el [] <| text "Content state link"]]
          ]
      Nothing -> text ""
    , case model.manifest of
      Just m -> 
          row 5 [fullWidth] 
          [ Button.light
                |> Button.content (TitleLine.iconOnly "copy")
                |> Button.onPress (CopyToClipboardInt m.id)
                |> Button.button
          , Html.a [Attributes.href m.id, Attributes.target "_blank"] [row 5 [fullWidth] [iiifIcon, el [] <| text "Manifest link"]]
          ]
      Nothing -> text ""
    , case (croppedUrlMaybe, fullImageUrlMaybe) of
        (Just url, _) -> 
          row 5 [fullWidth]
            [ Button.light
                  |> Button.content (TitleLine.iconOnly "copy")
                  |> Button.onPress (CopyToClipboardInt url)
                  |> Button.button
            , Html.a [Attributes.href url, Attributes.target "_blank"] [ row 5 [fullWidth] [TitleLine.iconOnly "image", el [] <| text "Cropped image"]]
            ]
        (_, Just url) ->
          row 5 [fullWidth]
            [ Button.light
                  |> Button.content (TitleLine.iconOnly "copy")
                  |> Button.onPress (CopyToClipboardInt url)
                  |> Button.button
            , Html.a [Attributes.href url, Attributes.target "_blank"] [ row 5 [fullWidth] [TitleLine.iconOnly "image", el [] <| text "Full image"]]
            ]
        _ -> text ""
    , case model.selection of 
        Nothing -> text ""
        _ -> row 5 []
              [ Button.light
                |> Button.content (TitleLine.withIcon "trash" "Clear selection")
                |> Button.onPress ClearSelection
                |> Button.button
              ]
    , case model.selection of
        Nothing -> el [] <| text "Select a canvas region on the left to target a specific region"
        _ -> row 5 [] [ el [] <| text "Selection label:", Html.input [Events.onInput SetLabel, Attributes.value model.label] [] ]
--    , case xywhTextMaybe of
--        Just xywhText ->
--          row 5 [fullWidth]
--            [ Button.light
--                      |> Button.content (TitleLine.iconOnly "copy")
--                      |> Button.onPress (CopyToClipboardInt xywhText)
--                      |> Button.button
--            , el [] <| text xywhText
--            ]
--        Nothing -> text ""
    , case contentStateMaybe of
        Just cs -> 
          row 5 [fullWidth]
          [ Button.light
                |> Button.content (TitleLine.iconOnly "copy")
                |> Button.onPress (CopyToClipboardInt cs)
                |> Button.button
          , column 5 [fullWidth] 
              [ el [] <| text "Content State (json):"
              , Html.textarea [Attributes.style "overflow" "auto", Attributes.rows 3, fullWidth] [text <| cs]
              ]
          ]
        Nothing -> text ""
    , case contentStateUrlMaybe of
        Just cs ->
          row 5 [fullWidth]
          [ Button.light
                |> Button.content (TitleLine.iconOnly "copy")
                |> Button.onPress (CopyToClipboardInt cs)
                |> Button.button
          , column 5 [fullWidth] 
              [ el [] <| text "Content State (base64url):"
              , Html.textarea [ Attributes.style "overflow" "auto", Attributes.rows 3, fullWidth] [text <| cs]
              ]
          ]
        Nothing -> text ""
    ]
  

