module Iiif.InternalUtils exposing(..)

import Dict exposing(Dict)

import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)

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
      , collections = Dict.foldl (\x -> dictInsert) a.collections b.collections
      , annotationLists = Dict.foldl (\x -> dictInsert) a.annotationLists b.annotationLists
      }

addManifest : Manifest -> Iiif -> Iiif
addManifest manifest iiif = 
  { iiif | manifests = dictInsert manifest iiif.manifests }

addCollection : Collection -> Iiif -> Iiif
addCollection collection iiif =
  { iiif | collections = dictInsert collection iiif.collections }

addAnnotationList : AnnotationList -> Iiif -> Iiif
addAnnotationList annotationList iiif =
  { iiif | annotationLists = dictInsert annotationList iiif.annotationLists }  

getObject : String -> (String -> Maybe String -> Maybe String -> b) -> Dict String b -> b
getObject key default dict =
  let maybe = Dict.get key dict
  in
    case maybe of
      Just obj -> obj
      Nothing -> default key Nothing Nothing


aliasManifest : Manifest -> ManifestUri -> Iiif -> Iiif
aliasManifest manifest aliasUri iiif =
  addManifest {manifest | id = aliasUri} iiif

aliasCollection : Collection -> CollectionUri -> Iiif -> Iiif
aliasCollection collection aliasUri iiif =
  addCollection {collection | id = aliasUri} iiif

aliasAnnotationList : AnnotationList -> AnnotationListUri -> Iiif -> Iiif
aliasAnnotationList annotationList aliasUri iiif =
  addAnnotationList {annotationList | id = aliasUri} iiif
