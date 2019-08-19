module Iiif.Utils exposing(..)

import Dict exposing(Dict)
import List.Extra as ListE

import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)
import Iiif.InternalUtils exposing(..)

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