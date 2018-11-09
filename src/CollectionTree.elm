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
import Config

type Msg  = OpenCollection CollectionUri
          | CloseCollection CollectionUri
          | SelectPath (List CollectionUri)
          | UrlChanged Url.Url
          | IiifNotification Iiif.Notification

type OutMsg = LoadManifest ManifestUri
            | LoadCollection CollectionUri

type alias Model =
  { url : Url.Url
  , iiif : Iiif
  , collections : List CollectionUri
  , openedCollections : Set CollectionUri
  , selectedCollection : Maybe CollectionUri
  , errors : List String
  }

init : Decode.Value -> Url.Url -> ( Model, Cmd Msg, List OutMsg )
init flags url =
  let 
    decodedRootUrl = Decode.decodeValue (Decode.field "rootUrl" Decode.string) flags
    baseModel = emptyModel url
    
    loadRoot = case decodedRootUrl of
      Result.Ok rootUrl -> 
        U.addOut [LoadCollection rootUrl] 
        >> U.mapModel (\m -> {m | collections = m.collections ++ [rootUrl]})
      Result.Err err -> identity
  in
    openUrl url baseModel |> loadRoot

emptyModel : Url.Url -> Model
emptyModel url =
  { url = url
  , iiif = Iiif.empty
  , collections = []
  , openedCollections = Set.empty
  , selectedCollection = Nothing
  , errors = []
  }

openUrl : Url.Url -> Model -> ( Model, Cmd Msg, List OutMsg )
openUrl url model = 
  case url.fragment of
    Nothing -> ( { model | url = url }, Cmd.none, [] )
    Just fragment -> 
      let 
        path = String.split "/" fragment
        fullPath = List.map Config.completeUri path
      in
        openPath fullPath { model | url = url }



openPath : List CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
openPath path model = 
  case path of 
    [] -> (model, Cmd.none, []) -- Shouldn't happen unless path is empty to start with
    [last] -> openCollection last {model | selectedCollection = Just last}
    elem :: elem2 :: rest -> 
      (model, Cmd.none, [])
        |> U.chain (openCollection elem)
        |> U.chain (openPath (elem2 :: rest))


fragmentPath : List Collection -> String
fragmentPath path = 
  path
    |> List.reverse
    |> List.map (Config.shortenUri << .id)
    |> String.join "/"
    |> (++) "#"


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

collectionTree : Model -> List Collection -> Collection -> Html Msg
collectionTree model path collection =
  div [ class "collection_node" ] 
    [ collectionTitleLine path collection model
    , case isCollectionOpened collection model of
        True ->
          case isStub collection of
            True -> 
              i [ class "spinner fas fa-spinner" ] []
            False -> 
              let
                subCollections = getCollections model.iiif collection.collections
              in
              div [ class "sub_collections" ] (List.map (collectionTree model (collection :: path)) subCollections)
        False -> span [] []
    ]

collectionTitleLine : List Collection -> Collection -> Model -> Html Msg
collectionTitleLine path collection model =
  let
    fullPath = collection :: path
  in
  span [ class <| "title_line" ++ (if model.selectedCollection == Just collection.id then " selected" else "") ] 
    [ Button.button [ Button.roleLink, Button.attrs [onClick (toggleOpenMessage collection model)] ] 
      [ if isCollectionOpened collection model then
          i [ class "icon fas fa-folder-open"] []
        else
          i [ class "icon fas fa-folder"] []
      ]
    , a [ href (fragmentPath fullPath)]
      [ span [ class "title" ] [ text <| Maybe.withDefault "Unnamed Collection" collection.label ]
      ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update msg model =
  case msg of
    OpenCollection targetUri -> openCollection targetUri model
    CloseCollection targetUri -> (setCollectionUriOpen targetUri False model, Cmd.none, [] )
--    SelectCollection targetUri -> openCollection {model | selectedCollection = Just targetUri} targetUri
    SelectPath path -> openPath path model
    UrlChanged url -> openUrl url model
    IiifNotification notification -> (model, Cmd.none, [])


openCollection : CollectionUri -> Model -> (Model, Cmd Msg, List OutMsg)
openCollection uri model =
  let 
    collection = getCollection model.iiif uri
    loadCollectionStep = 
      case isStub collection of 
        True -> U.addOut [LoadCollection collection.id]
        False -> identity
  in
    (model, Cmd.none, [])
      |> U.mapModel (setCollectionUriOpen uri True)
      |> loadCollectionStep
    


setCollectionOpen :  Collection -> Bool -> Model -> Model
setCollectionOpen collection value model =
  setCollectionUriOpen collection.id value model


setCollectionUriOpen : CollectionUri -> Bool -> Model -> Model
setCollectionUriOpen collectionUri value model =
  case value of
    True -> { model | openedCollections = insert collectionUri model.openedCollections }
    False -> { model | openedCollections = remove collectionUri model.openedCollections }
