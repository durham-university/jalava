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
  | CollectionPageLoadedInt CollectionUri (Result Http.Error (CollectionUri, Iiif))
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
         { model | iiif = manifestLoaded manifestUri iiif model.iiif }
         |> (\m -> if uri == manifestUri then m else {m | iiif = aliasManifest (getManifest m.iiif uri) manifestUri m.iiif} )
         |> (\m -> (m, Just (ManifestLoaded m.iiif manifestUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading manifest " ++ manifestUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    CollectionLoadedInt collectionUri res ->
      case res of
        Ok (uri, iiif) -> 
          { model | iiif = collectionLoaded collectionUri iiif model.iiif }
          |> (\m -> if uri == collectionUri then m else {m | iiif = aliasCollection (getCollection m.iiif uri) collectionUri m.iiif} )
          |> (\m -> (m, Just (CollectionLoaded m.iiif collectionUri)))
        Err e -> ({ model | errors = model.errors ++ ["Error loading collection " ++ collectionUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    CollectionPageLoadedInt pageUri res ->
      case res of
        Ok (collectionUri, iiif) -> 
          let
            maybeExisting = Dict.get collectionUri model.iiif.collections
          in
          case maybeExisting of
            Just existing ->
              let
                newModel =  if collectionNextPageUri existing == Just pageUri then
                              { model | iiif = mergePage collectionUri model.iiif iiif }
                            else
                              model
              in
                (newModel, Just (CollectionLoaded newModel.iiif collectionUri))
            Nothing -> (model, Nothing)
        Err e -> ({ model | errors = model.errors ++ ["Error loading collection page " ++ pageUri ++ ". " ++ (httpErrorToString e)] }, Nothing)
    AnnotationListLoadedInt annotationListUri res ->
      case res of
        Ok (uri, iiif) -> 
          { model | iiif = annotationListLoaded annotationListUri iiif model.iiif }
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
          LoadingPage -> (iiif, Cmd.none )
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
          LoadingPage -> (iiif, Cmd.none )
          Full -> ( iiif, Cmd.none )          

loadCollectionNextPage : CollectionUri -> Iiif -> (Iiif, Cmd Msg)
loadCollectionNextPage uri iiif =
  let
    maybeExisting = Dict.get uri iiif.collections
  in
    case maybeExisting of
      Nothing -> loadCollection uri iiif
      Just existing ->
        case existing.status of
          Stub -> loadCollection uri iiif
          Loading -> ( iiif, Cmd.none )
          LoadingPage -> (iiif, Cmd.none )
          Full ->
            case collectionNextPageUri existing of
              Nothing -> (iiif, Cmd.none)
              Just nextUri -> (iiif, Http.send (CollectionPageLoadedInt nextUri) (Http.get nextUri collectionDecoder))

collectionNextPageUri : Collection -> Maybe CollectionUri
collectionNextPageUri collection =
  case (collection.pageStatus, collection.firstPage, collection.nextPage) of
    (NoPages, _, _) -> Nothing
    (LastPage, _, _) -> Nothing
    (IndexPage, Just firstPage, _) -> Just firstPage
    (MorePages, _, Just nextPage) -> Just nextPage
    (_, _, _) -> Nothing


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
          LoadingPage -> ( iiif, Cmd.none )
          Full -> ( iiif, Cmd.none )


manifestLoaded : ManifestUri -> Iiif -> Iiif -> Iiif
manifestLoaded manifestUri loadedIiif oldIiif = merge oldIiif loadedIiif

collectionLoaded : CollectionUri -> Iiif -> Iiif -> Iiif
collectionLoaded collectionUri loadedIiif oldIiif = merge oldIiif loadedIiif

annotationListLoaded : AnnotationListUri -> Iiif -> Iiif -> Iiif
annotationListLoaded annotationListUri loadedIiif oldIiif = merge oldIiif loadedIiif