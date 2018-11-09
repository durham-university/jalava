module Iiif exposing(..)

import Dict exposing(Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional, hardcoded, custom)
import Json.Encode as Encode
import Http

type alias Uri = String

type alias ManifestUri = Uri

type alias CollectionUri = Uri

type alias SequenceUri = Uri

type alias CanvasUri = Uri

type alias ImageUri = Uri

type alias ResourceUri = Uri

type alias ServiceUri = Uri

type alias ManifestDict = Dict ManifestUri Manifest

type alias CollectionDict = Dict CollectionUri Collection

type Status = Stub | Loading | Full

type alias Manifest =
  { id : ManifestUri
  , label : Maybe String
  , description : Maybe String
  , logo : Maybe String
  , license : Maybe String
  , attribution : Maybe String
  , metadata : Dict String (List String)
  , related : Maybe ManifestLink
  , seeAlso : Maybe ManifestLink
  , sequences : List Sequence
  , status : Status
  }

type alias ManifestLink = 
  { id : Uri
  , label : Maybe String
  , format : Maybe String
  , profile : Maybe String
  }

type alias Collection =
  { id : CollectionUri
  , label : Maybe String
  , logo : Maybe String
  , collections : List CollectionUri
  , manifests : List ManifestUri
  , status : Status
  }

type alias Sequence =
  { id : SequenceUri
  , label : Maybe String
  , viewingDirection : Maybe String
  , viewingHint : Maybe String
  , canvases : List Canvas
  }

sequenceDecoder : Decode.Decoder Sequence
sequenceDecoder = 
  Decode.succeed Sequence
    |> required "@id" Decode.string
    |> optional "label" (Decode.nullable Decode.string) Nothing
    |> optional "viewingDirection" (Decode.nullable Decode.string) Nothing
    |> optional "viewingHint" (Decode.nullable Decode.string) Nothing
    |> optional "canvases" (Decode.list canvasDecoder) []

type alias Canvas =
  { id : CanvasUri
  , label : Maybe String
  , width : Int
  , height : Int
  , images : List Image
  }

canvasDecoder : Decode.Decoder Canvas
canvasDecoder = 
  Decode.succeed Canvas
    |> required "@id" Decode.string
    |> optional "label" (Decode.nullable Decode.string) Nothing
    |> required "width" Decode.int
    |> required "height" Decode.int
    |> optional "images" (Decode.list imageDecoder) []

type alias Image =
  { id : ImageUri
  , resource : Resource
  }

imageDecoder : Decode.Decoder Image
imageDecoder = 
  Decode.succeed Image
    |> required "@id" Decode.string
    |> required "resource" resourceDecoder

type alias Resource =
  { id : ResourceUri
  , format : Maybe String
  , width : Int
  , height : Int
  , service : Maybe Service
  }

resourceDecoder : Decode.Decoder Resource
resourceDecoder = 
  Decode.succeed Resource
    |> required "@id" Decode.string
    |> optional "format" (Decode.nullable Decode.string) Nothing
    |> required "width" Decode.int
    |> required "height" Decode.int
    |> optional "service" (Decode.nullable serviceDecoder) Nothing


type alias Service =
  { id : ServiceUri
  , profile : String
  }

serviceDecoder : Decode.Decoder Service
serviceDecoder = 
  Decode.succeed Service
    |> required "@id" Decode.string
    |> required "profile" Decode.string

type alias Iiif =
  { collections : CollectionDict
  , manifests : ManifestDict
  }

type Msg 
  = CollectionLoadedInt CollectionUri (Result Http.Error (CollectionUri, Iiif))
  | ManifestLoadedInt ManifestUri (Result Http.Error (ManifestUri, Iiif))

type Notification
  = ManifestLoaded ManifestUri
  | CollectionLoaded CollectionUri

--------------
-- DECODING --
--------------

stubManifest : ManifestUri -> Maybe String -> Maybe String -> Manifest
stubManifest id label logo =
  { id = id
  , label = label
  , description = Nothing
  , logo = logo
  , license = Nothing
  , attribution = Nothing
  , metadata = Dict.empty
  , related = Nothing
  , seeAlso = Nothing
  , sequences = []
  , status = Stub
  }

stubCollection : CollectionUri -> Maybe String -> Maybe String -> Collection
stubCollection id label logo =
  { id = id
  , label = label
  , logo = logo
  , collections = []
  , manifests = []
  , status = Stub
  }

manifestDecoder : Decode.Decoder (ManifestUri, Iiif)
manifestDecoder = 
  let
    manifest = Decode.succeed Manifest
      |> required "@id" Decode.string
      |> optional "label" (Decode.nullable Decode.string) Nothing
      |> optional "description" (Decode.nullable Decode.string) Nothing
      |> optional "logo" (Decode.nullable Decode.string) Nothing
      |> optional "license" (Decode.nullable Decode.string) Nothing
      |> optional "attribution" (Decode.nullable Decode.string) Nothing
      |> optional "metadata" (metadataDecoder) (Dict.empty)
      |> optional "related" (Decode.nullable manifestLinkDecoder) Nothing
      |> optional "seeAlso" (Decode.nullable manifestLinkDecoder) Nothing
      |> optional "sequences" (Decode.list sequenceDecoder) []
      |> hardcoded Full
    manifestUri = Decode.map .id manifest
    manifestDict = Decode.map2 Dict.singleton manifestUri manifest
    iiif = Decode.map (Iiif Dict.empty) manifestDict
  in
  Decode.map2 tuple2 manifestUri iiif


metadataDecoder : Decode.Decoder (Dict String (List String))
metadataDecoder =
  let 
    t2 = \a b -> (a, b)
    valueDecoder = Decode.oneOf
      [ Decode.list Decode.string
      , Decode.map (List.singleton) Decode.string
      ]
  in
  Decode.map Dict.fromList
    <| Decode.list (
      Decode.succeed t2
        |> required "label" Decode.string
        |> required "value" valueDecoder
    )

manifestLinkDecoder : Decode.Decoder (ManifestLink)
manifestLinkDecoder =
  Decode.succeed ManifestLink
    |> required "@id" Decode.string
    |> optional "label" (Decode.nullable Decode.string) Nothing
    |> optional "format" (Decode.nullable Decode.string) Nothing
    |> optional "profile" (Decode.nullable Decode.string) Nothing


manifestStubDecoder : Decode.Decoder Manifest
manifestStubDecoder = 
  Decode.succeed stubManifest
    |> required "@id" Decode.string
    |> optional "label" (Decode.nullable Decode.string) Nothing
    |> optional "logo" (Decode.nullable Decode.string) Nothing




maybeAttr maybeValue name = case maybeValue of
  Just value -> Just (name, Encode.string value)
  Nothing -> Nothing

tuple2 : a -> b -> (a, b)
tuple2 a b = (a, b)

collectionDecoder : Decode.Decoder (CollectionUri, Iiif)
collectionDecoder = 
  let 
    collection = Decode.succeed Collection
      |> required "@id" Decode.string
      |> optional "label" (Decode.nullable Decode.string) Nothing
      |> optional "logo" (Decode.nullable Decode.string) Nothing
      |> optional "collections" (Decode.list (Decode.field "@id" Decode.string)) []
      |> optional "manifests" (Decode.list (Decode.field "@id" Decode.string)) []
      |> hardcoded Full
    collectionUri = Decode.map .id collection
    subCollections = Decode.succeed Dict.fromList
      |> optional "collections" (Decode.list (Decode.map (\c -> (c.id, c)) collectionStubDecoder)) []
    allCollections = Decode.map2 (\c d -> Dict.insert c.id c d) collection subCollections
    allManifests = Decode.succeed Dict.fromList
      |> optional "manifests" (Decode.list (Decode.map (\m -> (m.id, m)) manifestStubDecoder)) []
    iiif = Decode.map2 Iiif allCollections allManifests
  in
  Decode.map2 tuple2 collectionUri iiif

collectionStubDecoder : Decode.Decoder Collection
collectionStubDecoder =
  Decode.succeed stubCollection
    |> required "@id" Decode.string
    |> optional "label" (Decode.nullable Decode.string) Nothing
    |> optional "logo" (Decode.nullable Decode.string) Nothing

----------
-- IIIF --
----------

empty : Iiif
empty = {
    collections = Dict.empty,
    manifests = Dict.empty
  }

dictInsert : { a | id : Uri, status : Status } -> Dict Uri { a | id : Uri, status : Status } -> Dict Uri { a | id : Uri, status : Status }
dictInsert obj dict =
  let existing = Dict.get obj.id dict
  in 
    case (obj.status, Maybe.map .status existing) of
      (Full, _)       -> Dict.insert obj.id obj dict
      (_, Nothing)    -> Dict.insert obj.id obj dict
      (Loading, Just Stub) -> Dict.insert obj.id obj dict
      (_, _) -> dict

merge : Iiif -> Iiif -> Iiif
merge  a b =
  {a  | manifests = Dict.foldl (\x -> dictInsert) a.manifests b.manifests
      , collections = Dict.foldl (\x -> dictInsert) a.collections b.collections }

addManifest : Manifest -> Iiif -> Iiif
addManifest manifest iiif = 
  { iiif | manifests = dictInsert manifest iiif.manifests }

addCollection : Collection -> Iiif -> Iiif
addCollection collection iiif =
  { iiif | collections = dictInsert collection iiif.collections }

getObject : String -> (String -> Maybe String -> Maybe String -> b) -> Dict String b -> b
getObject key default dict =
  let maybe = Dict.get key dict
  in
    case maybe of
      Just obj -> obj
      Nothing -> default key (Just "Error: Not found") Nothing


getManifest : Iiif -> ManifestUri -> Manifest
getManifest iiif manifestUri = getObject manifestUri stubManifest iiif.manifests

getManifests : Iiif -> List ManifestUri -> List Manifest
getManifests iiif = List.map (getManifest iiif)

getCollection : Iiif -> CollectionUri -> Collection
getCollection iiif collectionUri = getObject collectionUri stubCollection iiif.collections

getCollections : Iiif -> List CollectionUri -> List Collection
getCollections iiif = List.map (getCollection iiif)

toString : String -> { a | label : Maybe String } -> String
toString default obj =
  case obj.label of
    Just label -> label
    Nothing -> default

isStub : { a | status : Status } -> Bool
isStub obj = 
  case obj.status of
    Stub -> True
    Loading -> True
    Full -> False

manifestToString : Manifest -> String
manifestToString = toString "Unnamed manifest"

collectionToString : Collection -> String
collectionToString = toString "Unnamed collection"

-------------
-- Loading --
-------------

update : Msg -> { a | iiif : Iiif, errors : List String} -> ({ a | iiif : Iiif, errors : List String}, Maybe Notification)
update msg model =
  case msg of
    ManifestLoadedInt manifestUri res -> 
      case res of
        Ok (uri, iiif) -> ({ model | iiif = manifestLoaded iiif model.iiif }, Just (ManifestLoaded uri))
        Err e -> ({ model | errors = model.errors ++ ["Error loading manifest " ++ manifestUri] }, Nothing)
    CollectionLoadedInt collectionUri res ->
      case res of
        Ok (uri, iiif) -> ({ model | iiif = collectionLoaded iiif model.iiif }, Just (CollectionLoaded uri))
        Err e -> ({ model | errors = model.errors ++ ["Error loading collection " ++ collectionUri] }, Nothing)


loadManifest : ManifestUri -> Iiif -> (Iiif, Cmd Msg)
loadManifest uri iiif = 
  let 
    maybeExisting = Dict.get uri iiif.manifests
    cmd = Http.send (ManifestLoadedInt uri) (Http.get uri manifestDecoder)
  in
    case maybeExisting of
      Nothing ->
        let stub = stubManifest uri Nothing Nothing
        in ( addManifest { stub | status = Loading } iiif, cmd )
      Just existing ->
        case existing.status of
          Stub ->
            let loading = { existing | status = Loading }
            in ( addManifest loading iiif, cmd )
          Loading -> ( iiif, Cmd.none )
          Full -> ( iiif, Cmd.none )          


loadCollection : CollectionUri -> Iiif -> (Iiif, Cmd Msg)
loadCollection uri iiif = 
  let 
    maybeExisting = Dict.get uri iiif.collections
    cmd = Http.send (CollectionLoadedInt uri) (Http.get uri collectionDecoder)
  in
    case maybeExisting of
      Nothing ->
        let stub = stubCollection uri Nothing Nothing
        in ( addCollection { stub | status = Loading } iiif, cmd )
      Just existing ->
        case existing.status of
          Stub ->
            let loading = { existing | status = Loading }
            in ( addCollection loading iiif, cmd )
          Loading -> ( iiif, Cmd.none )
          Full -> ( iiif, Cmd.none )          


manifestLoaded : Iiif -> Iiif -> Iiif
manifestLoaded loadedIiif oldIiif = merge oldIiif loadedIiif

collectionLoaded : Iiif -> Iiif -> Iiif
collectionLoaded = manifestLoaded

---------------
-- Image API --
---------------

type Region 
  = FullRegion
  | Square
  | Box Int Int Int Int

type Size
  = FullSize
  | Max
  | Exactly Int Int
  | FitW Int
  | FitH Int
  | FitBox Int Int

type Rotation
  = NoRotation

type Quality
  = Default
  | Gray

canvasUrl : Size -> Canvas -> String
canvasUrl size canvas =
  case canvas.images of
    x :: xs -> imageUrl size x
    [] -> ""

imageUrl : Size -> Image -> String
imageUrl size image = imageUrlFull FullRegion size NoRotation Default image

imageUrlFull : Region -> Size -> Rotation -> Quality -> Image -> String
imageUrlFull region size rotation quality image =
  case image.resource.service of
    Nothing -> image.resource.id
    Just service ->
      imageServiceUrl region size rotation quality service

imageServiceUrl : Region -> Size -> Rotation -> Quality -> Service -> String
imageServiceUrl region size rotation quality service =
  let
    regionString = case region of
      FullRegion -> "full"
      Square -> "square"
      Box x y w h -> String.join "," (List.map String.fromInt [x,y,w,h])
    sizeString = case size of
      FullSize -> "full"
      Max -> "max"
      Exactly w h -> String.join "," (List.map String.fromInt [w,h])
      FitW w -> (String.fromInt w) ++ ","
      FitH h -> "," ++ (String.fromInt h)
      FitBox w h -> "!" ++ (String.join "," (List.map String.fromInt [w,h]))
    rotationString = case rotation of
      NoRotation -> "0"
    qualityString = case quality of
      Default -> "default"
      Gray -> "gray"
  in
    String.join "/" [service.id, regionString, sizeString, rotationString, qualityString]
