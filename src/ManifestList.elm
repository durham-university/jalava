module ManifestList exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Regex

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup

import Iiif exposing(..)
import ManifestDetails
import Utils exposing(iiifLink, pluralise, wrapKey)
import Update as U
import Config


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


view : Model -> Html msg
view model = 
  div [ class "manifest_list" ] (List.map (manifestLine model.iiif) (allManifestUris model))

manifestLine : Iiif -> ManifestUri -> Html msg
manifestLine iiif manifestUri =
  let 
    manifest = Iiif.getManifest iiif manifestUri
    maybeSequence = List.head manifest.sequences
    maybeAllCanvases = Maybe.map (.canvases) maybeSequence
    allCanvases = Maybe.withDefault [] maybeAllCanvases
    canvases = List.take 10 allCanvases
    logoHtml = case manifest.logo of
        Just logo -> [ img [src logo, class "logo"] [] ]
        Nothing -> []
    spinnerHtml = case isStub manifest of
      True -> [i [ class "spinner fas fa-spinner" ] []]
      False -> []
    re = Maybe.withDefault Regex.never (Regex.fromString "\\W+")
    details_id = Regex.replace re (\_ -> "_") manifest.id
  in
  Grid.row [] [ Grid.col [] [
    Card.config [ Card.attrs [class "manifest_preview_card"] ]
      |> Card.headerH3 [] (logoHtml ++ [ Html.a [href (Config.manifestViewerUrl manifest)] [text <| Iiif.manifestToString manifest] ] ++ spinnerHtml)
      |> Card.listGroup 
          [ ListGroup.li [] [ (canvasesLine manifest canvases) ]
          , ListGroup.li [ ListGroup.attrs [class "manifest_details collapse", id details_id] ] [ ManifestDetails.manifestDetails manifest ]
          ]
      |> Card.footer [] 
        [ div [ class "show_details"] 
          [ Button.button [Button.roleLink, Button.attrs [class "collapsed", attribute "data-toggle" "collapse", attribute "data-target" ("#" ++ details_id)]] [text "manifest details"] 
          ]
        , div [ class "image_count"] [ text <| pluralise (List.length allCanvases) "image" "images" ]
        , iiifLink manifestUri
        ]
      |> Card.view
  ]]

canvasesLine : Manifest -> List Canvas -> Html msg
canvasesLine manifest canvases =
-- Images need to be keyed. It appears that lazy loading confuses Elm otherwise.
  Keyed.node "div" [ class "canvas_line" ]
    (List.map (wrapKey <| \c -> a [ class "canvas_preview", href (Config.canvasViewerUrl manifest c) ] [ canvasImgHtml c ]) canvases )

canvasImgHtml : Canvas -> Html msg
canvasImgHtml canvas = 
  img [ class "lazyload", src "spinner_40x60.gif", attribute "data-src" <| Iiif.canvasUrl (Iiif.FitH 60) canvas] []
