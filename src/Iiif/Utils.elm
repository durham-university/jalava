module Iiif.Utils exposing(..)

import Dict exposing(Dict)
import List.Extra as ListE

import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)
import Iiif.InternalUtils exposing(..)

import Bytes.Encode
import Base64

type alias Rect =
  { x : Float
  , y : Float
  , w : Float
  , h : Float
  }

empty : Iiif
empty = 
  { collections = Dict.empty
  , manifests = Dict.empty
  , annotationLists = Dict.empty
  }
  

toString : String -> { a | label : Maybe String, status : Status } -> String
toString default obj =
  case obj.label of
    Just label -> label
    Nothing -> 
      case obj.status of
        Loading -> "Loading object"
        _ -> default


isStub : { a | status : Status } -> Bool
isStub obj = 
  case obj.status of
    Stub -> True
    Loading -> True
    LoadingPage -> False
    Full -> False
    Error _ -> True

willLoad : { a | status : Status } -> Bool
willLoad obj = 
  case obj.status of
    Stub -> True
    Loading -> True
    LoadingPage -> False
    Full -> False
    Error _ -> False

manifestToString : Manifest -> String
manifestToString = toString "Unnamed manifest"


collectionToString : Collection -> String
collectionToString = toString "Unnamed collection"


getManifest : Iiif -> ManifestUri -> Manifest
getManifest iiif manifestUri = getObject manifestUri stubManifest iiif.manifests

getManifests : Iiif -> List ManifestUri -> List Manifest
getManifests iiif = List.map (getManifest iiif)

getCollection : Iiif -> CollectionUri -> Collection
getCollection iiif collectionUri = getObject collectionUri stubCollection iiif.collections

getCollections : Iiif -> List CollectionUri -> List Collection
getCollections iiif = List.map (getCollection iiif)

getAnnotationList : Iiif -> AnnotationListUri -> AnnotationList
getAnnotationList iiif annotationListUri = getObject annotationListUri (\id _ _ -> stubAnnotationList id) iiif.annotationLists

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

getAnnotationLists : Iiif -> List AnnotationListUri -> List AnnotationList
getAnnotationLists iiif annotationListUris =
  List.map (getAnnotationList iiif) annotationListUris

getCanvasAnnotationLists : Canvas -> List AnnotationListUri
getCanvasAnnotationLists canvas =
  canvas.otherContent
    |> List.filter (\c -> c.contentType == Just "sc:AnnotationList")
    |> List.map .id


getCanvasAnnotation : Iiif -> Canvas -> AnnotationUri -> Maybe Annotation
getCanvasAnnotation iiif canvas annotationUri =
  getCanvasAnnotationLists canvas
    |> (getAnnotationLists iiif)
    |> List.map .annotations
    |> List.concat
    |> ListE.find (\a -> a.id == Just annotationUri)

contentStateJson : Manifest -> Maybe Canvas -> Maybe Rect -> Maybe String -> Maybe String
contentStateJson m canvasMaybe rectMaybe labelMaybe = 
  let
    xywhMaybe = rectMaybe |> Maybe.map (\rect -> [rect.x, rect.y, rect.w, rect.h] |> List.map (String.fromInt << round) |> String.join ",")
    label = labelMaybe |> Maybe.withDefault "Link target"
  in
  case (canvasMaybe, xywhMaybe) of
      (Just c, Just xywh) -> Just <|
        "{\"@context\": \"http://iiif.io/api/presentation/0/context.json\", \"id\": \"" ++ c.id ++ "_xywh_" ++ xywh ++ 
        "\", \"type\": \"Annotation\", \"motivation\": [\"contentState\"], \"resource\": {\"type\": \"dctypes:Text\", " ++ 
        "\"format\":\"text/plain\", \"chars\":\"" ++ label ++ "\"}, \"target\": {\"id\":\"" ++ c.id ++ "#xywh=" ++ xywh ++
        "\", \"type\":\"Canvas\", \"partOf\":{\"id\": \"" ++ m.id ++ "\",\"type\":\"Manifest\"}}}"
      (Just c, _) -> Just <|
        "{\"id\":\"" ++ c.id ++ "\", \"type\":\"Canvas\", \"partOf\":{\"id\": \"" ++ m.id ++ "\",\"type\":\"Manifest\"}}"
      _ -> Just <| "{\"id\":\"" ++ m.id ++ "\", \"type\":\"Manifest\"}"

contentState : Manifest -> Maybe Canvas -> Maybe Rect -> Maybe String -> Maybe String
contentState m canvasMaybe rectMaybe labelMaybe =
  contentStateJson m canvasMaybe rectMaybe labelMaybe
  |> Maybe.andThen (\cs -> Bytes.Encode.string cs |> Bytes.Encode.encode |> Base64.fromBytes)
  |> Maybe.map ((String.replace "+" "-") << (String.replace "/" "_") << (String.replace "=" ""))
