module Iiif.Loading exposing(..)

import Dict exposing(Dict)
import Http

import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)
import Iiif.Decoders exposing(..)
import Iiif.Utils exposing(..)
import Iiif.InternalUtils exposing(..)


type Msg 
  = CollectionLoadedInt CollectionUri (Result Http.Error (CollectionUri, Iiif))
  | ManifestLoadedInt ManifestUri (Result Http.Error (ManifestUri, Iiif))
  | AnnotationListLoadedInt AnnotationListUri (Result Http.Error (AnnotationListUri, Iiif))

type Notification
  = ManifestLoaded Iiif ManifestUri
  | CollectionLoaded Iiif CollectionUri
  | AnnotationListLoaded Iiif AnnotationListUri

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
         |> (\m -> (m, Just (ManifestLoaded m.iiif manifestUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading manifest " ++ manifestUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    CollectionLoadedInt collectionUri res ->
      case res of
        Ok (uri, iiif) -> 
          { model | iiif = collectionLoaded iiif model.iiif }
          |> (\m -> if uri == collectionUri then m else {m | iiif = aliasCollection (getCollection m.iiif uri) collectionUri m.iiif} )
          |> (\m -> (m, Just (CollectionLoaded m.iiif collectionUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading collection " ++ collectionUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    AnnotationListLoadedInt annotationListUri res ->
      case res of
        Ok (uri, iiif) -> 
          { model | iiif = annotationListLoaded iiif model.iiif }
          |> (\m -> if uri == annotationListUri then m else {m | iiif = aliasAnnotationList (getAnnotationList m.iiif uri) annotationListUri m.iiif} )
          |> (\m -> (m, Just (AnnotationListLoaded m.iiif annotationListUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading annotationList " ++ annotationListUri ++ ". " ++ (httpErrorToString e)] }, Nothing)


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


loadAnnotationList : AnnotationListUri -> Iiif -> (Iiif, Cmd Msg)
loadAnnotationList uri iiif =
  let
    maybeExisting = Dict.get uri iiif.annotationLists
    cmd = Http.send (AnnotationListLoadedInt uri) (Http.get uri annotationListDecoder)
  in
    case maybeExisting of
      Nothing ->
        let stub = stubAnnotationList uri
        in ( addAnnotationList { stub | status = Loading } iiif, cmd)
      Just existing ->
        case existing.status of
          Stub ->
            let loading = { existing | status = Loading}
            in ( addAnnotationList loading iiif, cmd )
          Loading -> ( iiif, Cmd.none )
          Full -> ( iiif, Cmd.none )


manifestLoaded : Iiif -> Iiif -> Iiif
manifestLoaded loadedIiif oldIiif = merge oldIiif loadedIiif

collectionLoaded : Iiif -> Iiif -> Iiif
collectionLoaded = manifestLoaded

annotationListLoaded : Iiif -> Iiif -> Iiif
annotationListLoaded = manifestLoaded