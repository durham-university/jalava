module Iiif.Decoders exposing(..)

import Dict exposing(Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional, hardcoded, custom)
import Json.Encode as Encode
import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)

sequenceDecoder : Decode.Decoder Sequence
sequenceDecoder = 
  Decode.succeed Sequence
    |> optional "@id" (Decode.nullable Decode.string) Nothing
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "viewingDirection" jsonLdValueStringDecoder Nothing
    |> optional "viewingHint" jsonLdValueStringDecoder Nothing
    |> optional "canvases" (Decode.list canvasDecoder) []

canvasDecoder : Decode.Decoder Canvas
canvasDecoder = 
  Decode.succeed Canvas
    |> required "@id" Decode.string
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> required "width" Decode.int
    |> required "height" Decode.int
    |> optional "images" (Decode.list annotationDecoder) []
    |> optional "thumbnail" (Decode.nullable resourceDecoder) Nothing
    |> optional "otherContent" (decodeListOrSingle otherContentDecoder) []


annotationDecoder : Decode.Decoder Annotation
annotationDecoder = 
  Decode.succeed Annotation
    |> optional "@id" (Decode.nullable Decode.string) Nothing
    |> optional "motivation" jsonLdValueStringDecoder Nothing
    |> required "resource" resourceDecoder
    |> optional "on" (decodeListOrSingleOrNull annotationOnDecoder |> Decode.map List.head) Nothing


annotationOnDecoder : Decode.Decoder AnnotationOn
annotationOnDecoder =
  Decode.oneOf
    [ Decode.string |> Decode.map (\s -> AnnotationOn s Nothing)
    , Decode.succeed AnnotationOn
        |> required "full" Decode.string
        |> optional "selector" (Decode.map Just annotationSelectorDecoder) Nothing
    ]

requiredValue : String -> Decode.Decoder String -> Decode.Decoder String
requiredValue expected decoder =
  let
    expectMapper : String -> Decode.Decoder String
    expectMapper x = 
      if x == expected then Decode.succeed x
      else Decode.fail ("Expected value " ++ expected)
  in decoder |> Decode.andThen expectMapper

annotationSelectorDecoder : Decode.Decoder Selector
annotationSelectorDecoder =
  Decode.oneOf 
    [ Decode.succeed (\_ default items -> ChoiceSelector { default = default, items = items })
        |> required "@type" (Decode.string |> requiredValue "oa:Choice" |> Decode.map Just)
        |> required "default" (Decode.lazy (\_ -> annotationSelectorDecoder))
        |> optional "item" (Decode.lazy (\_ -> decodeListOrSingle annotationSelectorDecoder)) []
    , Decode.succeed (\_ value -> FragmentSelector { value = value } )
        |> required "@type" (Decode.string |> requiredValue "oa:FragmentSelector")
        |> required "value" (jsonLdValueStringDecoder |> Decode.map (Maybe.withDefault ""))
    , Decode.succeed (\_ value -> SvgSelector { value = value } )
        |> required "@type" (Decode.string |> requiredValue "oa:SvgSelector")
        |> required "value" (jsonLdValueStringDecoder |> Decode.map (Maybe.withDefault ""))
    ]

resourceDecoder : Decode.Decoder Resource
resourceDecoder = 
  Decode.oneOf 
  [ Decode.succeed Resource
      |> optional "@id" (Decode.nullable Decode.string) Nothing
      |> optional "@type" (Decode.nullable Decode.string) Nothing
      |> optional "format" jsonLdValueStringDecoder Nothing
      |> optional "width" (Decode.nullable Decode.int) Nothing
      |> optional "height" (Decode.nullable Decode.int) Nothing
      |> optional "service" (Decode.nullable serviceDecoder) Nothing
      |> optional "chars" jsonLdValueStringDecoder Nothing
      |> optional "label" jsonLdValueStringDecoder Nothing
  , Decode.string |> Decode.map (\id -> Resource (Just id) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  ]


serviceDecoder : Decode.Decoder Service
serviceDecoder = 
  Decode.succeed Service
    |> required "@id" Decode.string
    |> required "profile" (decodeListOrSingle Decode.string)
    |> optional "width" (Decode.nullable Decode.int) Nothing
    |> optional "height" (Decode.nullable Decode.int) Nothing
    |> optional "sizes" (Decode.list serviceSizeDecoder) []


serviceSizeDecoder : Decode.Decoder (Int, Int)
serviceSizeDecoder =
  Decode.map2 (\x y -> (x, y))
    (Decode.field "width" Decode.int)
    (Decode.field "height" Decode.int)


rangeDecoder : Decode.Decoder Range
rangeDecoder =
  Decode.succeed Range
    |> required "@id" Decode.string
    |> optional "viewingHint" jsonLdValueStringDecoder Nothing
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "canvases" (Decode.list Decode.string) []
    |> optional "ranges" (Decode.list Decode.string) []



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


singleValueDecoder : Decode.Decoder a ->  Decode.Decoder (SingleValue a)
singleValueDecoder valueDecoder =
  Decode.oneOf
    [ valueDecoder |> Decode.map (\s -> SingleValue s Nothing Nothing)
    , Decode.succeed SingleValue
        |> required "@value" valueDecoder
        |> optional "@language" (Decode.nullable Decode.string) Nothing
        |> optional "@type" (Decode.nullable Decode.string) Nothing
    ]


otherContentDecoder : Decode.Decoder OtherContent
otherContentDecoder =
  Decode.succeed OtherContent
    |> required "@id" Decode.string
    |> optional "@type" (Decode.nullable Decode.string) Nothing
    |> optional "label" jsonLdValueStringDecoder Nothing


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
    iiif = Decode.map (\m -> Iiif Dict.empty m Dict.empty) manifestDict
  in
  Decode.map2 Tuple.pair manifestUri iiif


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
  Decode.succeed (\_ -> stubManifest)
    |> required "@type" (Decode.string |> requiredValue "sc:Manifest")
    |> required "@id" Decode.string
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "logo" jsonLdValueStringDecoder Nothing


decodeListOrSingle : Decode.Decoder a -> Decode.Decoder (List a)
decodeListOrSingle valueDecoder =
  Decode.oneOf
    [ valueDecoder |> Decode.map List.singleton
    , Decode.list valueDecoder
    ]

decodeListOrSingleOrNull : Decode.Decoder a -> Decode.Decoder (List a)
decodeListOrSingleOrNull valueDecoder =
  Decode.oneOf
    [ decodeListOrSingle valueDecoder
    , Decode.null []
    ]


maybeAttr maybeValue name = case maybeValue of
  Just value -> Just (name, Encode.string value)
  Nothing -> Nothing

collectionDecoder : Decode.Decoder (CollectionUri, Iiif)
collectionDecoder = 
  let
    subItems = Decode.succeed (\a b c -> a ++ b ++ c)
      |> optional "collections" (Decode.list collectionOrManifestStubDecoder) []
      |> optional "manifests" (Decode.list collectionOrManifestStubDecoder) []
      |> optional "members" (Decode.list collectionOrManifestStubDecoder) []
    
    collectionFilter : CollectionOrManifest -> Maybe Collection
    collectionFilter item =
      case item of
        JustCollection c -> Just c
        _ -> Nothing

    manifestFilter : CollectionOrManifest -> Maybe Manifest
    manifestFilter item =
      case item of
        JustManifest m -> Just m
        _ -> Nothing

    subCollections = Decode.map (List.filterMap collectionFilter) subItems

    manifests = Decode.map (List.filterMap manifestFilter) subItems

    collection = Decode.succeed Collection
      |> required "@id" Decode.string
      |> optional "label" jsonLdValueStringDecoder Nothing
      |> optional "logo" jsonLdValueStringDecoder Nothing
      |> custom (Decode.map (List.map .id) subCollections)
      |> custom (Decode.map (List.map .id) manifests)
      |> hardcoded Full
    collectionUri = Decode.map .id collection

    subCollectionsDict = subCollections
                          |> Decode.map (List.map (\c -> (c.id, c)))
                          |> Decode.map Dict.fromList    
    
    collectionsDict = Decode.map2 (\c d -> Dict.insert c.id c d) collection subCollectionsDict

    manifestsDict = manifests
                      |> Decode.map (List.map (\m -> (m.id, m)))
                      |> Decode.map Dict.fromList

    iiif = Decode.map2 (\c m -> Iiif c m Dict.empty) collectionsDict manifestsDict
  in
  Decode.map2 Tuple.pair collectionUri iiif

type CollectionOrManifest = JustCollection Collection
                          | JustManifest Manifest

collectionOrManifestStubDecoder : Decode.Decoder  CollectionOrManifest
collectionOrManifestStubDecoder =
  Decode.oneOf
    [ Decode.map JustCollection collectionStubDecoder
    , Decode.map JustManifest manifestStubDecoder
    ]

collectionStubDecoder : Decode.Decoder Collection
collectionStubDecoder =
  Decode.succeed (\_ -> stubCollection)
    |> required "@type" (Decode.string |> requiredValue "sc:Collection")
    |> required "@id" Decode.string
    |> optional "label" jsonLdValueStringDecoder Nothing
    |> optional "logo" jsonLdValueStringDecoder Nothing


annotationListDecoder : Decode.Decoder (AnnotationListUri, Iiif)
annotationListDecoder =
  let
    annotationList = Decode.succeed AnnotationList
      |> required "@id" Decode.string
      |> optional "label" jsonLdValueStringDecoder Nothing
      |> optional "resources" (Decode.list annotationDecoder) []
      |> hardcoded Full
    annotationListUri = Decode.map .id annotationList
    annotationListDict = Decode.map2 Dict.singleton annotationListUri annotationList
    iiif = Decode.map (Iiif Dict.empty Dict.empty) annotationListDict
  in
  Decode.map2 Tuple.pair annotationListUri iiif