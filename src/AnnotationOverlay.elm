port module AnnotationOverlay exposing(..)

import Browser
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, optional, hardcoded, custom)
import Iiif.Types exposing(..)
import Iiif.Utils
import Iiif.Decoders

import Html exposing (..)
import Html.Attributes as HAttrs
import Svg exposing (..)
import Svg.Attributes as SAttrs
import Svg.Events as SEvents
import VirtualDom as Dom

import XmlParser exposing (..)

type Msg  = SetAnnotations (Maybe Canvas) (List Annotation)
          | OnMouseOver AnnotationUri
          | OnMouseOut AnnotationUri

port inPortSetAnnotations : (Decode.Value -> msg) -> Sub msg

port outPortShowAnnotation : Maybe AnnotationUri -> Cmd msg

type alias Model =
  { canvas : Maybe Canvas 
  , annotations : List Annotation 
  }

emptyModel : Model
emptyModel =
  { canvas = Nothing
  , annotations = []
  }

main : Program Decode.Value Model Msg
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

subscriptions : Model -> Sub Msg
subscriptions model = inPortSetAnnotations decodeSetAnnotations

decodeSetAnnotations : Decode.Value -> Msg
decodeSetAnnotations value =
  let decoded = Decode.decodeValue setAnnotationsDecoder value
  in
  case decoded of
    Ok msg -> msg
    Err error -> SetAnnotations Nothing [] -- TODO: error messages

setAnnotationsDecoder : Decode.Decoder Msg
setAnnotationsDecoder =
  Decode.succeed SetAnnotations
    |> optional "canvas" (Decode.nullable Iiif.Decoders.canvasDecoder) Nothing
    |> required "annotations" (Decode.list Iiif.Decoders.annotationDecoder)

init : Decode.Value -> (Model, Cmd Msg)
init flags = (emptyModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    SetAnnotations canvas annotations -> 
      ( { model | canvas = canvas, annotations = annotations }, Cmd.none)
    OnMouseOver annotationUri -> 
      ( model, outPortShowAnnotation (Just annotationUri) )
    OnMouseOut annotationUri -> 
      ( model, outPortShowAnnotation Nothing )


withPopover : AnnotationUri -> (List (Dom.Attribute Msg) -> Dom.Node Msg) -> Dom.Node Msg
withPopover annotationUri htmlF =
  htmlF 
    [SEvents.on "mouseover" (Decode.succeed (OnMouseOver annotationUri))
    ,SEvents.on "mouseout" (Decode.succeed (OnMouseOut annotationUri))
    ]

xmlNodeToSvg : AnnotationUri -> XmlParser.Node -> List (Svg Msg)
xmlNodeToSvg annotationUri node = 
  case node of
    XmlParser.Element elem attributes children ->
      case elem of
        "svg" -> List.concatMap (xmlNodeToSvg annotationUri) children
        "path" -> [withPopover annotationUri (\attrs -> Svg.path ((List.map (\a -> Dom.attribute a.name a.value) attributes) ++ attrs) [])]
        _ -> []
    _ -> []


xmlToSvg : AnnotationUri -> XmlParser.Xml -> List (Svg Msg)
xmlToSvg annotationUri xml = xmlNodeToSvg annotationUri xml.root

findSvgSelector : List Selector -> Maybe String
findSvgSelector l =
  case l of
    [] -> Nothing
    (SvgSelector v) :: xs -> Just v.value
    x :: xs -> findSvgSelector xs

selectorSvg : AnnotationUri -> Selector -> List (Svg Msg)
selectorSvg annotationUri selector =
  case selector of 
    SvgSelector s ->
      case XmlParser.parse s.value of
        Ok xml -> xmlToSvg annotationUri xml
        Err error -> [] -- TODO: errors
    ChoiceSelector s ->
      let
        maybeSvgSelector = findSvgSelector s.items
      in
        case maybeSvgSelector of
          Just value -> selectorSvg annotationUri (SvgSelector {value = value})
          Nothing -> selectorSvg annotationUri s.default
    FragmentSelector s -> []

annotationSvg : Annotation -> List (Svg Msg)
annotationSvg annotation =
  let
    selector = annotation.on |> Maybe.andThen .selector
    content = annotation.resource.chars |> Maybe.withDefault ""
  in
    case (selector, annotation.id) of 
      (Just s, Just annotationUri) -> selectorSvg annotationUri s
      (_, _) -> []

view : Model -> Html Msg
view model =
  let 
    w = Maybe.map .width model.canvas |> Maybe.withDefault 1000 |> String.fromInt
    h = Maybe.map .height model.canvas |> Maybe.withDefault 1000 |> String.fromInt
  in
  div [HAttrs.id "annotation_overlay"] 
    [ svg [ SAttrs.viewBox ("0 0 " ++ w ++ " " ++ h) ] <| 
        List.concatMap annotationSvg model.annotations
--    , div [ HAttrs.attribute "style" "position: absolute; left: 0; top: 0; width: 100%; height: 100%;"] [Html.text "Moomoo"]
    ]
