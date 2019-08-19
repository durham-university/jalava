module Iiif.Types exposing(..)

import Dict exposing(Dict)

type alias Uri = String

type alias ManifestUri = Uri

type alias CollectionUri = Uri

type alias SequenceUri = Uri

type alias CanvasUri = Uri

type alias AnnotationListUri = Uri

type alias AnnotationUri = Uri

type alias ResourceUri = Uri

type alias ServiceUri = Uri

type alias RangeUri = Uri

type alias ManifestDict = Dict ManifestUri Manifest

type alias CollectionDict = Dict CollectionUri Collection

type alias AnnotationListDict = Dict AnnotationListUri AnnotationList

type alias Iiif =
  { collections : CollectionDict
  , manifests : ManifestDict
  , annotationLists : AnnotationListDict
  }

type Status = Stub | Loading | LoadingPage | Full

type PageStatus = NoPages | IndexPage | MorePages | LastPage

type alias Manifest =
  { id : ManifestUri
  , label : Maybe String
  , description : Maybe String
  , logo : Maybe Resource
  , license : Maybe String
  , attribution : Maybe String
  , metadata : Dict String (List String)
  , related : List ManifestLink
  , seeAlso : List ManifestLink
  , sequences : List Sequence
  , structures : List Range
  , status : Status
  }

type alias Collection =
  { id : CollectionUri
  , label : Maybe String
  , description : Maybe String
  , logo : Maybe Resource
  , collections : List CollectionUri
  , manifests : List ManifestUri
  , status : Status
  , pageStatus : PageStatus
  , firstPage : Maybe CollectionUri
  , nextPage : Maybe CollectionUri
  }

type alias AnnotationList =
  { id : AnnotationListUri
  , label : Maybe String
  , annotations : List Annotation
  , status : Status
  }

type alias ManifestLink = 
  { id : Uri
  , label : Maybe String
  , format : Maybe String
  , profile : Maybe String
  }

type alias Sequence =
  { id : Maybe SequenceUri
  , label : Maybe String
  , viewingDirection : Maybe String
  , viewingHint : Maybe String
  , canvases : List Canvas
  }

type alias Canvas =
  { id : CanvasUri
  , label : Maybe String
  , width : Int
  , height : Int
  , images : List Annotation
  , thumbnail : Maybe Resource
  , otherContent : List OtherContent
  }

type alias Annotation =
  { id : Maybe AnnotationUri
  , motivation : Maybe String
  , resource : Resource
  , on : Maybe AnnotationOn
  }

type alias AnnotationOn =
  { full : Uri
  , selector : Maybe Selector
  }

type Selector = ChoiceSelector { default : Selector, items : List Selector }
              | FragmentSelector { value : String }
              | SvgSelector { value : String }

type alias Resource =
  { id : Maybe ResourceUri
  , resourceType : Maybe String
  , format : Maybe String
  , width : Maybe Int
  , height : Maybe Int
  , service : Maybe Service
  , chars : Maybe String
  , label : Maybe String
  }

type alias Service =
  { id : ServiceUri
  , profile : List String
  , width : Maybe Int
  , height : Maybe Int
  , sizes : List (Int, Int)
  }

type alias Range =
  { id : RangeUri 
  , viewingHint : Maybe String
  , label : Maybe String
  , canvases : List CanvasUri
  , ranges : List RangeUri
  }

type alias OtherContent =
  { id : String
  , contentType : Maybe String
  , label : Maybe String
  }

type alias SingleValue a =
  { value : a
  , language : Maybe String
  , valueType : Maybe String
  }

type alias JsonLdValue a = List (SingleValue a)
