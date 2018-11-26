module CollectionTree exposing(Model, Msg(..), OutMsg(..), component, init, view, update, emptyModel)

import Set exposing(Set, insert, remove, member)
import Url
import Json.Decode as Decode
import Html exposing(..)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy

import Iiif.Types exposing(..)
import Iiif.Loading
import Iiif.Utils exposing(getCollection, getCollections, isStub)

import UI.Core exposing(..)
import UI.Tree as Tree
import UI.Icon as Icon

import Utils exposing(..)
import Update as U

type Msg  = OpenCollection CollectionUri
          | CloseCollection CollectionUri
          | SelectPath (List CollectionUri)
          | CollectionClicked (List CollectionUri)
          | IiifNotification Iiif.Loading.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri
            | CollectionSelected (List CollectionUri)

type alias Model =
  { iiif : Iiif
  , collections : List CollectionUri
  , openedCollections : Set CollectionUri
  , selectedCollection : List CollectionUri
  , errors : List String
  }

component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = \x -> Sub.none }

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags =
  let 
    decodedRootUrl = Decode.decodeValue (Decode.field "rootUrl" Decode.string) flags
    baseModel = emptyModel
    
    loadRoot = case decodedRootUrl of
      Result.Ok rootUrl -> 
        U.addOut [LoadCollection rootUrl] 
        >> U.mapModel (\m -> {m | collections = m.collections ++ [rootUrl]})
      Result.Err err -> identity
  in
    (baseModel, Cmd.none, []) |> loadRoot

emptyModel : Model
emptyModel =
  { iiif = Iiif.Utils.empty
  , collections = []
  , openedCollections = Set.empty
  , selectedCollection = []
  , errors = []
  }


openPath : List CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
openPath path model = 
  ({model | selectedCollection = path}, Cmd.none, [])
    |> U.fold openCollection path


toggleOpenMessage : Model -> Collection -> Msg
toggleOpenMessage model collection = 
  case isCollectionOpened collection model of
    True -> CloseCollection collection.id
    False -> OpenCollection collection.id

isCollectionOpened : Collection -> Model -> Bool
isCollectionOpened collection model = isCollectionUriOpened collection.id model

isCollectionUriOpened : CollectionUri -> Model -> Bool
isCollectionUriOpened collectionUri model = member collectionUri model.openedCollections

view : Model -> Html Msg
view model = Lazy.lazy view_ model

view_ : Model -> Html Msg
view_ model = 
  let
    collectionToNode : List CollectionUri -> Collection -> (Collection, List CollectionUri)
    collectionToNode path collection = (collection, collection.id :: path)

    nodeOpen : (Collection, List CollectionUri) -> Bool
    nodeOpen (collection, path) = isCollectionOpened collection model

    nodeLabel : (Collection, List CollectionUri) -> Html msg
    nodeLabel (collection, path) = el [Attributes.style "white-space" "no-wrap"] <| text (Iiif.Utils.toString "Unnamed collection" collection)

    nodeIcon : (Collection, List CollectionUri) -> Maybe (Html msg)
    nodeIcon (collection, path) = 
      if isCollectionOpened collection model then Just (Icon.icon "folder-open" [])
      else Just (Icon.icon "folder"[])

    nodeChildren : (Collection, List CollectionUri) -> List (Collection, List CollectionUri)
    nodeChildren (collection, path) = List.map (collectionToNode path) (getCollections model.iiif collection.collections)

    nodeSelected : (Collection, List CollectionUri) -> Bool
    nodeSelected (collection, path) = List.head model.selectedCollection == List.head path
  in
  Tree.empty
    |> Tree.attributes [fullWidth]
    |> Tree.rootItems (List.map (collectionToNode []) (getCollections model.iiif model.collections))
    |> Tree.label nodeLabel
    |> Tree.icon nodeIcon
    |> Tree.children nodeChildren
    |> Tree.selected nodeSelected
    |> Tree.open nodeOpen
    |> Tree.onPress (Just << CollectionClicked << Tuple.second)
    |> Tree.onPressIcon (Just << (toggleOpenMessage model) << Tuple.first)
    |> Tree.tree


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    OpenCollection targetUri -> openCollection targetUri model
    CloseCollection targetUri -> (setCollectionUriOpen targetUri False model, Cmd.none, [] )
    SelectPath path -> openPath path model
    CollectionClicked path -> 
      openPath path model |> U.addOut [CollectionSelected path]
    IiifNotification notification -> 
      case notification of
        Iiif.Loading.CollectionLoaded iiif collectionUri -> ({model | iiif = iiif}, Cmd.none, [])
        _ -> (model, Cmd.none, [])


openCollection : CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
openCollection uri model =
  let 
    collection = getCollection model.iiif uri
    loadCollection = 
      case isStub collection of 
        True -> U.addOut [LoadCollection collection.id]
        False -> identity
  in
    (model, Cmd.none, [])
      |> U.mapModel (setCollectionUriOpen uri True)
      |> loadCollection
    

setCollectionOpen :  Collection -> Bool -> Model -> Model
setCollectionOpen collection value model =
  setCollectionUriOpen collection.id value model


setCollectionUriOpen : CollectionUri -> Bool -> Model -> Model
setCollectionUriOpen collectionUri value model =
  case value of
    True -> { model | openedCollections = insert collectionUri model.openedCollections }
    False -> { model | openedCollections = remove collectionUri model.openedCollections }
