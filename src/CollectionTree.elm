module CollectionTree exposing(Model, Msg(..), OutMsg(..), init, view, update, emptyModel)

import Set exposing(Set, insert, remove, member)
import Url
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button

import Iiif exposing(..)
import Utils exposing(..)
import Update as U

type Msg  = OpenCollection CollectionUri
          | CloseCollection CollectionUri
          | SelectPath (List CollectionUri)
          | CollectionClicked (List CollectionUri)
          | IiifNotification Iiif.Notification

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
  { iiif = Iiif.empty
  , collections = []
  , openedCollections = Set.empty
  , selectedCollection = []
  , errors = []
  }


openPath : List CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
openPath path model = 
  ({model | selectedCollection = path}, Cmd.none, [])
    |> U.fold openCollection path


toggleOpenMessage : Collection -> Model -> Msg
toggleOpenMessage collection model = 
  case isCollectionOpened collection model of
    True -> CloseCollection collection.id
    False -> OpenCollection collection.id

isCollectionOpened : Collection -> Model -> Bool
isCollectionOpened collection model = isCollectionUriOpened collection.id model

isCollectionUriOpened : CollectionUri -> Model -> Bool
isCollectionUriOpened collectionUri model = member collectionUri model.openedCollections


view : Model -> Html Msg
view model = 
  let collections = getCollections model.iiif model.collections
  in
  div [ class "collection_tree"] <|
    (List.map (collectionTree model []) collections)

collectionTree : Model -> List CollectionUri -> Collection -> Html Msg
collectionTree model path collection =
  div [ class "collection_node" ] 
    [ collectionTitleLine path collection model
    , case isCollectionOpened collection model of
        True ->
          case isStub collection of
            True -> spinner
            False -> 
              let
                subCollections = getCollections model.iiif collection.collections
              in
              div [ class "sub_collections" ] (List.map (collectionTree model (collection.id :: path)) subCollections)
        False -> span [] []
    ]

collectionTitleLine : List CollectionUri -> Collection -> Model -> Html Msg
collectionTitleLine path collection model =
  let
    fullPath = collection.id :: path
  in
  span [ class <| "title_line" ++ (if List.head model.selectedCollection == Just collection.id then " selected" else "") ] 
    [ Button.button [ Button.roleLink, Button.attrs [onClick (toggleOpenMessage collection model)] ] 
      [ if isCollectionOpened collection model then
          i [ class "icon fas fa-folder-open"] []
        else
          i [ class "icon fas fa-folder"] []
      ]
    , Button.button [ Button.roleLink, Button.attrs [onClick (CollectionClicked fullPath)]]
      [ span [ class "title" ] [ text <| Maybe.withDefault "Unnamed Collection" collection.label ]
      ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    OpenCollection targetUri -> openCollection targetUri model
    CloseCollection targetUri -> (setCollectionUriOpen targetUri False model, Cmd.none, [] )
    SelectPath path -> openPath path model
    CollectionClicked path -> 
      openPath path model |> U.addOut [CollectionSelected path]
    IiifNotification notification -> (model, Cmd.none, [])


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
