module Iiif.Encoders exposing(..)

import Iiif.Types exposing(..)
import Json.Encode as Encode


field : String -> a -> (a -> Encode.Value) -> List (String, Encode.Value) -> List (String, Encode.Value)
field fieldName value encoder valueList =
  (fieldName, encoder value) :: valueList

fieldMaybe : String -> Maybe a -> (a -> Encode.Value) -> List (String, Encode.Value) -> List (String, Encode.Value)
fieldMaybe fieldName valueMaybe encoder valueList =
  case valueMaybe of
    Just value -> field fieldName value encoder valueList
    Nothing -> valueList

nonEmptyList : List a -> Maybe (List a)
nonEmptyList list = 
  if List.isEmpty list then Nothing
  else Just list


canvasEncoder : Canvas -> Encode.Value
canvasEncoder canvas =
  []
  |> field "@id" canvas.id Encode.string
  |> fieldMaybe "label" canvas.label Encode.string
  |> field "width" canvas.width Encode.int
  |> field "height" canvas.height Encode.int
  |> fieldMaybe "images" (nonEmptyList canvas.images) (Encode.list annotationEncoder)
  |> fieldMaybe "thumbnail" canvas.thumbnail resourceEncoder
  |> fieldMaybe "otherContent" (nonEmptyList canvas.otherContent) (Encode.list otherContentEncoder)
  |> Encode.object


otherContentEncoder : OtherContent -> Encode.Value
otherContentEncoder otherContent =
  []
  |> field "@id" otherContent.id Encode.string
  |> fieldMaybe "@type" otherContent.contentType Encode.string
  |> fieldMaybe "label" otherContent.label Encode.string
  |> Encode.object


annotationEncoder : Annotation -> Encode.Value
annotationEncoder annotation =
  []
  |> fieldMaybe "@id" annotation.id Encode.string
  |> fieldMaybe "motivation" annotation.motivation Encode.string
  |> field "resource" annotation.resource resourceEncoder
  |> fieldMaybe "on" annotation.on annotationOnEncoder
  |> Encode.object


annotationOnEncoder : AnnotationOn -> Encode.Value
annotationOnEncoder annotationOn =
  []
  |> field "full" annotationOn.full Encode.string
  |> fieldMaybe "selector" annotationOn.selector annotationSelectorEncoder
  |> Encode.object


annotationSelectorEncoder : Selector -> Encode.Value
annotationSelectorEncoder selector =
  case selector of 
    ChoiceSelector s ->
      []
      |> field "@type" "oa:Choice" Encode.string
      |> field "default" s.default annotationSelectorEncoder
      |> field "item" s.items (Encode.list annotationSelectorEncoder)
      |> Encode.object
    FragmentSelector s -> 
      []
      |> field "@type" "oa:FragmentSelector" Encode.string
      |> field "value" s.value Encode.string
      |> Encode.object
    SvgSelector s ->
      []
      |> field "@type" "oa:SvgSelector" Encode.string
      |> field "value" s.value Encode.string
      |> Encode.object


resourceEncoder : Resource -> Encode.Value
resourceEncoder resource =
  []
  |> fieldMaybe "@id" resource.id Encode.string
  |> fieldMaybe "@type" resource.resourceType Encode.string
  |> fieldMaybe "width" resource.width Encode.int
  |> fieldMaybe "height" resource.height Encode.int
  |> fieldMaybe "service" resource.service serviceEncoder
  |> fieldMaybe "chars" resource.chars Encode.string
  |> fieldMaybe "label" resource.label Encode.string
  |> Encode.object


serviceEncoder : Service -> Encode.Value
serviceEncoder service =
  []
  |> field "@id" service.id Encode.string
  |> fieldMaybe "profile" (nonEmptyList service.profile) (Encode.list Encode.string)
  |> fieldMaybe "width" service.width Encode.int
  |> fieldMaybe "height" service.height Encode.int
  |> fieldMaybe "sizes" (nonEmptyList service.sizes) (Encode.list serviceSizeEncoder)
  |> Encode.object


serviceSizeEncoder : (Int, Int) -> Encode.Value
serviceSizeEncoder (w, h) =
  []
  |> field "width" w Encode.int
  |> field "height" h Encode.int
  |> Encode.object
