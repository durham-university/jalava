module Iiif.Stubs exposing(..)

import Iiif.Types exposing(..)
import Dict exposing(Dict)

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

stubAnnotationList : AnnotationListUri -> AnnotationList
stubAnnotationList id =
  { id = id
  , label = Nothing
  , annotations = []
  , status = Stub
  }