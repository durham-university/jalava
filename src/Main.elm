module Main exposing(..)

import Browser
import Browser.Navigation as Nav

import Html as Html exposing(Html)
import Html.Attributes as Attributes exposing(style)

import Http
import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing(Set)

import Iiif.Types exposing(..)
import Iiif.Loading exposing(loadManifest, loadCollection, loadAnnotationList, loadCollectionNextPage)
import Iiif.Utils exposing(getManifest, getCollection)

import Utils exposing(..)

import UriMapper exposing (UriMapper)
import Update as U

import MainElement

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , uriMapper : UriMapper
  , mainElementModel : MainElement.Model
  }

type OutMsg = Impossible

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | MainElementMsg MainElement.Msg

mainElement = 
  U.subComponent 
    { component = MainElement.component 
    , unwrapModel = .mainElementModel
    , wrapModel = \model subModel -> { model | mainElementModel = subModel }
    , wrapMsg = MainElementMsg
    , outEvaluator = \msgSub model ->
        case msgSub of
          MainElement.UpdateUrl -> 
            (model, Cmd.none, [])
            |> U.chain2 updateUrl
    }

main : Program Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    decodedUriMapper = Decode.decodeValue (Decode.field "uriMapper" (UriMapper.uriMapperDecoder UriMapper.base)) flags
    setUriMapper = case decodedUriMapper of
      Result.Ok uriMapper -> U.mapModel (\m -> {m | uriMapper = uriMapper })
      Result.Err err -> U.mapModel (\m -> m) -- {m | errors = m.errors ++ [Decode.errorToString err] })
  in
  (emptyModel url key, Cmd.none, [])
    |> setUriMapper
    |> U.chain (mainElement.init flags)
    |> U.chain2 (parseUrl url)
    |> U.ignoreOut


parseUrl : Url.Url -> Model -> (Model, Cmd Msg)
parseUrl url model =
  let 
    (pathFragment, viewFragment) =
      case Maybe.map (String.split "!") url.fragment of
        Nothing -> (Nothing, Nothing)
        Just [] -> (Nothing, Nothing)
        Just [x] -> (Just x, Nothing)
        Just (x :: y :: xs) -> (Just x, Just y)
    path =
      case pathFragment of
        Nothing -> []
        Just fragment ->
          String.split "/" fragment
          |> List.map model.uriMapper.inflate
          |> List.reverse
    (selectedManifest, selectedCanvas) = 
      case Maybe.map (String.split "/") viewFragment of
        Nothing -> (Nothing, Nothing)
        Just [] -> (Nothing, Nothing)
        Just [x] -> (Just (model.uriMapper.inflate x), Nothing)
        Just (x :: y :: xs) -> (Just (model.uriMapper.inflate x), Just (model.uriMapper.inflate y))
    newModel = { model | url = url }
  in
    (newModel, Cmd.none, [])
    |> U.chain (mainElement.updater <| MainElement.UrlUpdated path selectedManifest selectedCanvas)
    |> U.ignoreOut



updateUrl : Model -> (Model, Cmd Msg)
updateUrl model = 
  let
    treePath = 
      model.mainElementModel.collectionTreeModel.selectedCollection
      |> List.reverse
      |> List.map model.uriMapper.deflate
      |> String.join "/"
    viewManifest = 
      [Maybe.map .id model.mainElementModel.manifestViewModel.manifest, model.mainElementModel.manifestViewModel.canvas]
      |> List.filterMap identity
      |> List.map model.uriMapper.deflate
      |> String.join "/"
    newFragment = 
      case model.mainElementModel.screen of
        MainElement.Browser -> treePath
        MainElement.Viewer -> treePath ++ "!" ++ viewManifest
    oldUrl = model.url
    newUrl = { oldUrl | fragment = Just newFragment }
  in 
    ({ model | url = newUrl }, Nav.pushUrl model.key (Url.toString newUrl))


emptyModel : Url.Url -> Nav.Key -> Model
emptyModel url key = 
  { key = key
  , url = url
  , uriMapper = UriMapper.empty
  , mainElementModel = MainElement.emptyModel
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url -> 
      if url == model.url then (model, Cmd.none)
      else parseUrl url model 
    MainElementMsg subMsg -> 
      (model, Cmd.none, [])
        |> U.chain (mainElement.updater subMsg)
        |> U.ignoreOut


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ mainElement.subscriptions model
  ]


view : Model -> Browser.Document Msg
view model =
  { title = "Jalava"
  , body = [ mainElement.view model ]
  }
