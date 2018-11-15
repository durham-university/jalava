module Iiif exposing(..)

import Dict exposing(Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional, hardcoded, custom)
import Json.Encode as Encode
import Http
import List.Extra as ListE

type alias Uri = String

type alias ManifestUri = Uri

type alias CollectionUri = Uri

type alias SequenceUri = Uri

type alias CanvasUri = Uri

type alias ImageUri = Uri

type alias ResourceUri = Uri

type alias ServiceUri = Uri

type alias RangeUri = Uri

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
  , related : List ManifestLink
  , seeAlso : List ManifestLink
  , sequences : List Sequence
  , structures : List Range
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
  { id : Maybe SequenceUri
  , label : Maybe String
  , viewingDirection : Maybe String
  , viewingHint : Maybe String
  , canvases : List Canvas
  }

sequenceDecoder : Decode.Decoder Sequence
sequenceDecoder = 
  Decode.succeed Sequence
    |> optional "@id" (Decode.nullable Decode.string) Nothing
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "viewingDirection" jsonLdValueStringDecoder Nothing
    |> optional "viewingHint" jsonLdValueStringDecoder Nothing
    |> optional "canvases" (Decode.list canvasDecoder) []

type alias Canvas =
  { id : CanvasUri
  , label : Maybe String
  , width : Int
  , height : Int
  , images : List Image
  , thumbnail : Maybe Resource
  }

canvasDecoder : Decode.Decoder Canvas
canvasDecoder = 
  Decode.succeed Canvas
    |> required "@id" Decode.string
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> required "width" Decode.int
    |> required "height" Decode.int
    |> optional "images" (Decode.list imageDecoder) []
    |> optional "thumbnail" (Decode.nullable resourceDecoder) Nothing


type alias Image =
  { id : Maybe ImageUri
  , resource : Resource
  }

imageDecoder : Decode.Decoder Image
imageDecoder = 
  Decode.succeed Image
    |> optional "@id" (Decode.nullable Decode.string) Nothing
    |> required "resource" resourceDecoder

type alias Resource =
  { id : ResourceUri
  , format : Maybe String
  , width : Maybe Int
  , height : Maybe Int
  , service : Maybe Service
  }

resourceDecoder : Decode.Decoder Resource
resourceDecoder = 
  Decode.oneOf 
  [ Decode.succeed Resource
      |> required "@id" Decode.string
      |> optional "format" jsonLdValueStringDecoder Nothing
      |> optional "width" (Decode.nullable Decode.int) Nothing
      |> optional "height" (Decode.nullable Decode.int) Nothing
      |> optional "service" (Decode.nullable serviceDecoder) Nothing
  , Decode.string |> Decode.map (\id -> Resource id Nothing Nothing Nothing Nothing)
  ]


type alias Service =
  { id : ServiceUri
  , profile : List String
  , width : Maybe Int
  , height : Maybe Int
  , sizes : List (Int, Int)
  }

serviceDecoder : Decode.Decoder Service
serviceDecoder = 
  Decode.succeed Service
    |> required "@id" Decode.string
    |> required "profile" (Decode.oneOf [
          Decode.string |> Decode.map List.singleton,
          Decode.list Decode.string
        ])
    |> optional "width" (Decode.nullable Decode.int) Nothing
    |> optional "height" (Decode.nullable Decode.int) Nothing
    |> optional "sizes" (Decode.list serviceSizeDecoder) []


serviceSizeDecoder : Decode.Decoder (Int, Int)
serviceSizeDecoder =
  Decode.map2 (\x y -> (x, y))
    (Decode.field "width" Decode.int)
    (Decode.field "height" Decode.int)

type alias Range =
  { id : RangeUri 
  , viewingHint : Maybe String
  , label : Maybe String
  , canvases : List CanvasUri
  , ranges : List RangeUri
  }

rangeDecoder : Decode.Decoder Range
rangeDecoder =
  Decode.succeed Range
    |> required "@id" Decode.string
    |> optional "viewingHint" jsonLdValueStringDecoder Nothing
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "canvases" (Decode.list Decode.string) []
    |> optional "ranges" (Decode.list Decode.string) []


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

type alias JsonLdValue a = List (SingleValue a)

jsonLdValueDecoder : Decode.Decoder a -> Decode.Decoder (JsonLdValue a)
jsonLdValueDecoder valueDecoder =
  Decode.oneOf
    [ singleValueDecoder valueDecoder |> Decode.map List.singleton
    , Decode.list (singleValueDecoder valueDecoder)
    ]

jsonLdValueStringDecoder : Decode.Decoder (Maybe String)
jsonLdValueStringDecoder =
  (jsonLdValueDecoder Decode.string)
    |> Decode.map List.head
    |> Decode.map (Maybe.map .value)

jsonLdValueStringListDecoder : Decode.Decoder (List String)
jsonLdValueStringListDecoder =
  (jsonLdValueDecoder Decode.string)
    |> Decode.map (List.map .value)

type alias SingleValue a =
  { value : a
  , language : Maybe String
  , valueType : Maybe String
  }

singleValueDecoder : Decode.Decoder a ->  Decode.Decoder (SingleValue a)
singleValueDecoder valueDecoder =
  Decode.oneOf
    [ valueDecoder |> Decode.map (\s -> SingleValue s Nothing Nothing)
    , Decode.succeed SingleValue
        |> required "@value" valueDecoder
        |> optional "@language" (Decode.nullable Decode.string) Nothing
        |> optional "@type" (Decode.nullable Decode.string) Nothing
    ]
  


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
  , related = []
  , seeAlso = []
  , sequences = []
  , structures = []
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
      |> optional "label" jsonLdValueStringDecoder Nothing
      |> optional "description" jsonLdValueStringDecoder Nothing
      |> optional "logo" jsonLdValueStringDecoder Nothing
      |> optional "license" jsonLdValueStringDecoder Nothing
      |> optional "attribution" jsonLdValueStringDecoder Nothing
      |> optional "metadata" (metadataDecoder) (Dict.empty)
      |> optional "related" manifestLinkDecoder []
      |> optional "seeAlso" manifestLinkDecoder []
      |> optional "sequences" (Decode.oneOf
                                [ Decode.list sequenceDecoder
                                , Decode.map List.singleton sequenceDecoder
                                ]) []
      |> optional "structures" (Decode.list rangeDecoder) []
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
  in
  Decode.map Dict.fromList
    <| Decode.list (
      Decode.succeed t2
        |> required "label" (Decode.map (Maybe.withDefault "") jsonLdValueStringDecoder)
        |> required "value" jsonLdValueStringListDecoder
    )

manifestLinkDecoder : Decode.Decoder (List ManifestLink)
manifestLinkDecoder =
  let 
    fullDecoder = Decode.succeed ManifestLink
      |> required "@id" Decode.string
      |> optional "label" jsonLdValueStringDecoder Nothing
      |> optional "format" jsonLdValueStringDecoder Nothing
      |> optional "profile" jsonLdValueStringDecoder Nothing
    simpleDecoder = Decode.string |> Decode.map (\s -> { id = s, label = Nothing, format = Nothing, profile = Nothing})
  in
  Decode.oneOf 
    [ fullDecoder |> Decode.map List.singleton
    , simpleDecoder |> Decode.map List.singleton
    , Decode.list fullDecoder
    , Decode.list simpleDecoder
    ]


manifestStubDecoder : Decode.Decoder Manifest
manifestStubDecoder = 
  Decode.succeed stubManifest
    |> required "@id" Decode.string
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "logo" jsonLdValueStringDecoder Nothing




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
      |> optional "label" jsonLdValueStringDecoder Nothing
      |> optional "logo" jsonLdValueStringDecoder Nothing
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
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "logo" jsonLdValueStringDecoder Nothing

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


aliasManifest : Manifest -> ManifestUri -> Iiif -> Iiif
aliasManifest manifest aliasUri iiif =
  addManifest {manifest | id = aliasUri} iiif

aliasCollection : Collection -> CollectionUri -> Iiif -> Iiif
aliasCollection collection aliasUri iiif =
  addCollection {collection | id = aliasUri} iiif


getManifest : Iiif -> ManifestUri -> Manifest
getManifest iiif manifestUri = getObject manifestUri stubManifest iiif.manifests

getManifests : Iiif -> List ManifestUri -> List Manifest
getManifests iiif = List.map (getManifest iiif)

getCollection : Iiif -> CollectionUri -> Collection
getCollection iiif collectionUri = getObject collectionUri stubCollection iiif.collections

getCollections : Iiif -> List CollectionUri -> List Collection
getCollections iiif = List.map (getCollection iiif)

getRange : Manifest -> RangeUri -> Maybe Range
getRange manifest rangeUri =
  let
    findRange : List Range -> Maybe Range
    findRange list =
      case list of
        [] -> Nothing
        x :: xs -> if x.id == rangeUri then Just x else findRange xs
  in
    findRange manifest.structures

getRanges : Manifest -> List RangeUri -> List Range
getRanges manifest rangeUris = 
  List.map (getRange manifest) rangeUris
  |> List.filterMap identity

getTopRanges : Manifest -> List Range
getTopRanges manifest =
  if List.length manifest.structures == 0 then []
  else
    let withViewingHint = List.filter (\r -> r.viewingHint == Just "top") manifest.structures
    in
      if List.length withViewingHint > 0 then withViewingHint
      else manifest.structures

getCanvas : Manifest -> CanvasUri -> Maybe Canvas
getCanvas manifest canvasUri = 
  let 
    findCanvas : List Canvas -> Maybe Canvas
    findCanvas list = 
      case list of
        [] -> Nothing
        x :: xs -> if x.id == canvasUri then Just x else findCanvas xs
  in
    List.map (findCanvas << .canvases) manifest.sequences
    |> List.filterMap identity 
    |> List.head

osdSourceImage : Image -> String
osdSourceImage image =
  case image.resource.service of
    -- TODO: Should check service profile
    Just service -> service.id ++ "/info.json"
    Nothing -> image.resource.id

osdSource : Canvas -> Maybe String
osdSource canvas = 
  List.head canvas.images
  |> Maybe.map osdSourceImage

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

httpErrorToString : Http.Error -> String
httpErrorToString e =
  case e of
    Http.BadUrl url -> "Bad url " ++ url
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network error"
    Http.BadPayload msg _ -> "Error parsing response" -- msg tends to be massive so don't include it
    Http.BadStatus _ -> "Bad response code"

update : Msg -> { a | iiif : Iiif, errors : List String} -> ({ a | iiif : Iiif, errors : List String}, Maybe Notification)
update msg model =
  case msg of
    ManifestLoadedInt manifestUri res -> 
      case res of
        Ok (uri, iiif) -> 
         { model | iiif = manifestLoaded iiif model.iiif }
         |> (\m -> if uri == manifestUri then m else {m | iiif = aliasManifest (getManifest m.iiif uri) manifestUri m.iiif} )
         |> (\m -> (m, Just (ManifestLoaded manifestUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading manifest " ++ manifestUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    CollectionLoadedInt collectionUri res ->
      case res of
        Ok (uri, iiif) -> 
          { model | iiif = collectionLoaded iiif model.iiif }
          |> (\m -> if uri == collectionUri then m else {m | iiif = aliasCollection (getCollection m.iiif uri) collectionUri m.iiif} )
          |> (\m -> (m, Just (CollectionLoaded collectionUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading collection " ++ collectionUri ++ ". " ++ (httpErrorToString e)] }, Nothing)


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

canvasThumbnailUrl : Size -> Canvas -> String
canvasThumbnailUrl size canvas =
  case canvas.thumbnail of
    Just thumbnail ->resourceUrl FullRegion size NoRotation Default thumbnail
    Nothing -> case canvas.images of
                  x :: xs -> imageUrl size x
                  [] -> ""

imageUrl : Size -> Image -> String
imageUrl size image = imageUrlFull FullRegion size NoRotation Default image

imageUrlFull : Region -> Size -> Rotation -> Quality -> Image -> String
imageUrlFull region size rotation quality image =
  resourceUrl region size rotation quality image.resource

resourceUrl : Region -> Size -> Rotation -> Quality -> Resource -> String
resourceUrl region size rotation quality resource =
  case resource.service of
    Nothing -> resource.id
    Just service ->
      imageServiceUrl region size rotation quality "jpg" service

matchSizeToAvailable : Service -> Size -> Size
matchSizeToAvailable service size =
  if List.length service.sizes == 0 then size
  else
    let 
      sorted = List.sortBy Tuple.second service.sizes
      -- default shouldn't happen, already checked that list isn't empty
      max = (ListE.last sorted) |> Maybe.withDefault (0,0)
      matched = case size of
        FullSize -> max
        Max -> max
        Exactly w h -> ListE.find (\s -> Tuple.first s >= w && Tuple.second s >= h) sorted |> Maybe.withDefault max
        FitW w -> ListE.find (\s -> Tuple.first s >= w) sorted |> Maybe.withDefault max
        FitH h -> ListE.find (\s -> Tuple.second s >= h) sorted |> Maybe.withDefault max
        FitBox w h -> ListE.find (\s -> Tuple.first s >= w && Tuple.second s >= h) sorted |> Maybe.withDefault max
    in Exactly (Tuple.first matched) (Tuple.second matched)


imageServiceUrl : Region -> Size -> Rotation -> Quality -> String -> Service -> String
imageServiceUrl region size rotation quality format service =
  let
    regionString = case region of
      FullRegion -> "full"
      Square -> "square"
      Box x y w h -> String.join "," (List.map String.fromInt [x,y,w,h])
    sizeString = case (matchSizeToAvailable service size) of
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
    (String.join "/" [service.id, regionString, sizeString, rotationString, qualityString]) ++ "." ++ format
