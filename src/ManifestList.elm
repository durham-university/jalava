module ManifestList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Debug

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

import Iiif exposing(..)
import Utils exposing(iiifLink, pluralise)
import Update as U
import UriMapper


type alias Model = 
  { iiif : Iiif
  , manifests : List ManifestUri
  , collection : Maybe CollectionUri
  , selectedManifest : Maybe ManifestUri
  , errors : List String
  }

type Msg = AddManifest ManifestUri
         | ClearManifests
         | SetManifests (List ManifestUri)
         | SetCollection CollectionUri
         | ClearCollection
         | IiifNotification Iiif.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

emptyModel = 
  { iiif = Iiif.empty
  , manifests = []
  , collection = Nothing
  , selectedManifest = Nothing
  , errors = []
  }

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model = 
  case msg of
    AddManifest manifestUri -> updateLoadManifests { model | manifests = model.manifests ++ [manifestUri]}
    ClearManifests -> ({ model | manifests = [] }, Cmd.none, [])
    SetManifests manifestUris -> updateLoadManifests { model | manifests = manifestUris }
    ClearCollection -> ({ model | collection = Nothing }, Cmd.none, [])
    SetCollection collectionUri -> updateLoadManifests { model | collection = Just collectionUri }
    IiifNotification notification -> 
      case notification of
        Iiif.CollectionLoaded collectionUri -> 
          if model.collection == Just collectionUri then
            updateLoadManifests model
          else
            (model, Cmd.none, [])
        _ -> (model, Cmd.none, [])
    


allManifestUris : Model -> List ManifestUri
allManifestUris model = 
  let 
    maybeCollection = Maybe.map (getCollection model.iiif) model.collection
    maybeManifestUris = Maybe.map .manifests maybeCollection
    collectionManifests = Maybe.withDefault [] maybeManifestUris
  in
    model.manifests ++ collectionManifests

updateLoadManifests : Model -> (Model, Cmd Msg, List OutMsg)
updateLoadManifests model =
  let 
    stubManifests = List.filter isStub (getManifests model.iiif (allManifestUris model))
    stubUris = List.map .id stubManifests
  in
    (model, Cmd.none, [])
      |> U.foldOut (\uri m -> [LoadManifest uri]) stubUris


manifestViewerUrl : Manifest -> String
manifestViewerUrl manifest = 
  let shortId = UriMapper.shortenUri manifest.id
  in "https://iiif.durham.ac.uk/index.html?manifest=" ++ shortId

canvasViewerUrl : Manifest -> Canvas -> String
canvasViewerUrl manifest canvas =
  let 
    manifestUrl = manifestViewerUrl manifest
    shortId = UriMapper.shortenUri canvas.id
  in manifestUrl ++ "&canvas=" ++ shortId


view : Model -> Html msg
view model = 
  div [ class "manifest_list" ] (List.map (manifestLine model.iiif) (allManifestUris model))

manifestLine : Iiif -> ManifestUri -> Html msg
manifestLine iiif manifestUri =
  let 
    manifest = Iiif.getManifest iiif manifestUri
    sequences = manifest.sequences
    canvases = case sequences of
      x :: xs -> List.take 10 x.canvases
      [] -> []
    logoHtml = case manifest.logo of
        Just logo -> [ img [src logo, class "logo"] [] ]
        Nothing -> []
    spinnerHtml = case isStub manifest of
      True -> [i [ class "spinner fas fa-spinner" ] []]
      False -> []
  in
  Grid.row [] [ Grid.col [] [
    Card.config [ Card.attrs [class "manifest_preview_card"] ]
      |> Card.headerH3 [] (logoHtml ++ [ Html.a [href (manifestViewerUrl manifest)] [text <| Iiif.manifestToString manifest] ] ++ spinnerHtml)
      |> Card.block [] 
          (List.map (\c -> Block.link [ href (canvasViewerUrl manifest c) ] [ img [src <| Iiif.canvasUrl (Iiif.FitH 60) c] [] ]) canvases )
      |> Card.footer [ class "text-muted" ] [ text <| pluralise (List.length canvases) "image - " "images - ", iiifLink manifestUri ]
      |> Card.view
  ]]
