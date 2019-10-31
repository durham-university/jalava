port module AnnotationOverlay exposing(..)

import SelectionTypes exposing(..)

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
          | OnMouseOverAnnotation AnnotationUri
          | OnMouseOutAnnotation AnnotationUri
          | OnCanvasDrag DragData
          | SelectionMessage SelectionOverlayMsg

port inPortSetAnnotations : (Decode.Value -> msg) -> Sub msg

port inPortSelectionMsg : (Decode.Value -> msg) -> Sub msg

port inPortCanvasMouse : (Decode.Value -> msg) -> Sub msg

port outPortShowAnnotation : Maybe AnnotationUri -> Cmd msg

port outPortCaptureDrag : Bool -> Cmd msg

port outPortSetSelection : Decode.Value -> Cmd msg

type DragStatus = NoDrag
                | Drag (DragData -> Rect)

type alias DragData =
  { x1: Int
  , y1: Int
  , x2: Int
  , y2: Int
  , state: String
  }

type alias Model =
  { canvas : Maybe Canvas 
  , annotations : List Annotation 
  , selectionEnabled : Bool
  , selection : Maybe Rect
  , dragging : DragStatus
  , capturing : Bool
  }

emptyModel : Model
emptyModel =
  { canvas = Nothing
  , annotations = []
  , selectionEnabled = False
  , selection = Nothing
  , capturing = False
  , dragging = NoDrag
  }

main : Program Decode.Value Model Msg
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch 
    [ inPortSetAnnotations decodeSetAnnotations
    , inPortCanvasMouse decodeDragData
    , inPortSelectionMsg decodeSelectionMsg
    ]


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


decodeSelectionMsg : Decode.Value -> Msg
decodeSelectionMsg value =
  let decoded = Decode.decodeValue selectionOverlayMsgDecoder value
  in
  case decoded of
    Ok msg -> SelectionMessage msg
    Err error -> SelectionMessage <| SetSelectionEnabled False


decodeDragData : Decode.Value -> Msg
decodeDragData value =
  let decoded = Decode.decodeValue dragDataDecoder value
  in
  case decoded of
    Ok msg -> msg
    Err error -> OnCanvasDrag {x1 = -1, y1 = -1, x2 = -1, y2 = -1, state = "error"}


dragDataDecoder : Decode.Decoder Msg
dragDataDecoder =
  Decode.succeed DragData
    |> required "x1" Decode.int
    |> required "y1" Decode.int
    |> required "x2" Decode.int
    |> required "y2" Decode.int
    |> required "state" Decode.string
    |> Decode.map OnCanvasDrag

init : Decode.Value -> (Model, Cmd Msg)
init flags = (emptyModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    SetAnnotations canvas annotations -> 
      ( { model | canvas = canvas, annotations = annotations }, Cmd.none)
    OnMouseOverAnnotation annotationUri -> 
      ( model, outPortShowAnnotation (Just annotationUri) )
    OnMouseOutAnnotation annotationUri -> 
      ( model, outPortShowAnnotation Nothing )
    OnCanvasDrag dragData ->
      let 
        rect =  { x = toFloat <| min dragData.x1 dragData.x2
                , y = toFloat <| min dragData.y1 dragData.y2
                , w = toFloat <| abs <| dragData.x2 - dragData.x1
                , h = toFloat <| abs <| dragData.y2 - dragData.y1
                }
      in 
      model
        |> startDrag dragData
        |> endDrag dragData
        |>  ( \m -> case m.dragging of
                  NoDrag -> m
                  Drag f -> { m | selection = Just <| f dragData }
            )
        |> setCaptureDragCmd
        |>  ( \(m, c) -> case m.dragging of
                  NoDrag -> (m, c)
                  Drag _ -> addSetSelectionCmd (m, c)
            )
    SelectionMessage smsg -> 
      case smsg of
        SetSelectionEnabled status -> setCaptureDragCmd {model | selectionEnabled = status }
        ClearSelection -> setCaptureDragCmd {model | selection = Nothing}

startDrag : DragData -> Model -> Model
startDrag dragData model = 
  let
    toRect : Float -> Float -> Float -> Float -> Rect
    toRect x1 y1 x2 y2 =  { x = min x1 x2
                          , y = min y1 y2
                          , w = abs <| x2 - x1
                          , h = abs <| y2 - y1
                          }

    toRectInt : Int -> Int -> Int -> Int -> Rect
    toRectInt x1 y1 x2 y2 = toRect (toFloat x1) (toFloat y1) (toFloat x2) (toFloat y2)

    isInside : (Int, Int) -> Rect -> Bool
    isInside (x, y) rect = 
      let 
        xf = toFloat x
        yf = toFloat y
      in
      xf >= rect.x && xf < rect.x + rect.w && yf >= rect.y && yf < rect.y + rect.h
  in
  if dragData.state == "start" && model.selectionEnabled then
    case model.selection of
      Just rect -> 
        let
          d = rectDivision rect
          p = (dragData.x1, dragData.y1)
        in
        if      isInside p d.tl then 
          { model | dragging = Drag (\dd -> toRect (min (rect.x + (toFloat <| dd.x2 - dd.x1)) (rect.x + rect.w)) (min (rect.y + (toFloat <| dd.y2 - dd.y1)) (rect.y + rect.h)) (rect.x + rect.w) (rect.y + rect.h) ) }
        else if isInside p d.t  then 
          { model | dragging = Drag (\dd -> toRect (rect.x) (min (rect.y + (toFloat <| dd.y2 - dd.y1)) (rect.y + rect.h)) (rect.x + rect.w) (rect.y + rect.h) ) }
        else if isInside p d.tr then 
          { model | dragging = Drag (\dd -> toRect (rect.x) (min (rect.y + (toFloat <| dd.y2 - dd.y1)) (rect.y + rect.h)) (rect.x + max 0 (rect.w + (toFloat <| dd.x2 - dd.x1))) (rect.y + rect.h) ) }
        else if isInside p d.l  then
          { model | dragging = Drag (\dd -> toRect (min (rect.x + (toFloat <| dd.x2 - dd.x1)) (rect.x + rect.w)) (rect.y) (rect.x + rect.w) (rect.y + rect.h) ) }
        else if isInside p d.c  then 
          { model | dragging = Drag (\dd -> toRect (rect.x + (toFloat <| dd.x2 - dd.x1)) (rect.y + (toFloat <| dd.y2 - dd.y1)) (rect.x + rect.w + (toFloat <| dd.x2 - dd.x1)) (rect.y + rect.h + (toFloat <| dd.y2 - dd.y1)) ) }
        else if isInside p d.r  then
          { model | dragging = Drag (\dd -> toRect (rect.x) (rect.y) (rect.x + max 0 (rect.w + (toFloat <| dd.x2 - dd.x1))) (rect.y + rect.h) ) }
        else if isInside p d.bl then
          { model | dragging = Drag (\dd -> toRect (min (rect.x + (toFloat <| dd.x2 - dd.x1)) (rect.x + rect.w)) (rect.y) (rect.x + rect.w) (rect.y + max 0 (rect.h + (toFloat <| dd.y2 - dd.y1))) ) }
        else if isInside p d.b  then
          { model | dragging = Drag (\dd -> toRect (rect.x) (rect.y) (rect.x + rect.w) (rect.y + max 0 (rect.h + (toFloat <| dd.y2 - dd.y1))) ) }
        else if isInside p d.br then
          { model | dragging = Drag (\dd -> toRect (rect.x) (rect.y) (rect.x + max 0 (rect.w + (toFloat <| dd.x2 - dd.x1))) (rect.y + max 0 (rect.h + (toFloat <| dd.y2 - dd.y1))) ) }
        else model
      Nothing -> { model | dragging = Drag (\dd -> toRectInt dd.x1 dd.y1 dd.x2 dd.y2) }
  else model

endDrag : DragData -> Model -> Model
endDrag dragData model =
  if dragData.state == "end" then
    { model | dragging = NoDrag }
  else model

setCaptureDragCmd : Model -> (Model, Cmd Msg)
setCaptureDragCmd model =
  let 
    capture =  case (model.selectionEnabled, model.selection, model.dragging) of
      (_, _, Drag _) -> True
      (True, Nothing, NoDrag) -> True
      _ -> False
  in
    ({model | capturing = capture}, outPortCaptureDrag capture)


addSetSelectionCmd : (Model, Cmd Msg) -> (Model, Cmd Msg)
addSetSelectionCmd (model, cmd) =
  (model, Cmd.batch [cmd, outPortSetSelection (encodeSelection model.selection)])


withPopover : AnnotationUri -> (List (Dom.Attribute Msg) -> Dom.Node Msg) -> Dom.Node Msg
withPopover annotationUri htmlF =
  htmlF 
    [ SEvents.on "mouseover" (Decode.succeed (OnMouseOverAnnotation annotationUri))
    , SEvents.on "mouseout" (Decode.succeed (OnMouseOutAnnotation annotationUri))
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


rectDivision : Rect -> {tl: Rect, t: Rect, tr: Rect, l: Rect, c: Rect, r: Rect, bl: Rect, b: Rect, br: Rect}
rectDivision rect =
  let
    x1 = rect.x
    x2 = rect.x + rect.w * 0.2
    x3 = rect.x + rect.w * 0.8
    x4 = rect.x + rect.w
    y1 = rect.y
    y2 = rect.y + rect.h * 0.2
    y3 = rect.y + rect.h * 0.8
    y4 = rect.y + rect.h

    rect2 x y xx yy = Rect x y (xx - x) (yy - y)
  in
  { tl = rect2 x1 y1 x2 y2
  , t  = rect2 x2 y1 x3 y2
  , tr = rect2 x3 y1 x4 y2
  , l  = rect2 x1 y2 x2 y3
  , c  = rect2 x2 y2 x3 y3
  , r  = rect2 x3 y2 x4 y3
  , bl = rect2 x1 y3 x2 y4
  , b  = rect2 x2 y3 x3 y4
  , br = rect2 x3 y3 x4 y4  
  }


selectionOverlay : Model -> List (Svg Msg)
selectionOverlay model = 
  let
    outliteAttrs rect = 
      [ SAttrs.x <| String.fromFloat (rect.x + 3)
      , SAttrs.y <| String.fromFloat (rect.y + 3)
      , SAttrs.width <| String.fromFloat <| max (rect.w - 6) 0
      , SAttrs.height <| String.fromFloat <| max (rect.h - 6) 0
      , SAttrs.strokeWidth "6"
      , SAttrs.fill "none"
      ]

    highlightAttrs rect =
      [ SAttrs.strokeWidth "0"
      , SAttrs.class "selectionHandle"
      , SAttrs.strokeOpacity "0.0"
      , SAttrs.fill "white"
      , SAttrs.x <| String.fromFloat rect.x
      , SAttrs.y <| String.fromFloat rect.y
      , SAttrs.width <| String.fromFloat rect.w
      , SAttrs.height <| String.fromFloat rect.h
      ]

    buildPath : List (String, Float, Float) -> String
    buildPath path = 
      List.map (\(c, x, y) -> c ++ (String.fromFloat x) ++ " " ++ (String.fromFloat y)) path |> String.join " "
  in
  if model.selectionEnabled then
    let
      w = Maybe.map .width model.canvas |> Maybe.withDefault 1000 |> String.fromInt
      h = Maybe.map .height model.canvas |> Maybe.withDefault 1000 |> String.fromInt
      selectionRect = 
        case model.selection of
          Nothing -> []
          Just rect -> 
            let
              d = rectDivision rect
            in
              [ Svg.rect (highlightAttrs d.tl) []
              , Svg.rect (highlightAttrs d.t) []
              , Svg.rect (highlightAttrs d.tr) []
              , Svg.rect (highlightAttrs d.l) []
              , Svg.rect (highlightAttrs d.c) []
              , Svg.rect (highlightAttrs d.r) []
              , Svg.rect (highlightAttrs d.bl) []
              , Svg.rect (highlightAttrs d.b) []
              , Svg.rect (highlightAttrs d.br) []
              , Svg.path 
                [ SAttrs.d <| buildPath 
                                [ ("M", d.t.x, d.t.y), ("L", d.b.x, d.b.y + d.b.h)
                                , ("M", d.tr.x, d.tr.y), ("L", d.br.x, d.br.y + d.br.h)
                                , ("M", d.l.x, d.l.y), ("L", d.r.x + d.r.w, d.r.y)
                                , ("M", d.bl.x, d.bl.y), ("L", d.br.x + d.br.w, d.br.y)
                                ]
                , SAttrs.stroke "white"
                , SAttrs.strokeWidth "4"
                ] []
              , Svg.rect ((outliteAttrs rect) ++
                  [ SAttrs.stroke "black"
                  , SAttrs.strokeDasharray "20,20"
                  ]) []
              , Svg.rect ((outliteAttrs rect) ++
                  [ SAttrs.stroke "white"
                  , SAttrs.strokeWidth "2"
                  , SAttrs.strokeDasharray "20,20"
                  , SAttrs.strokeDashoffset "20"
                  ]) []
              ]
    in
      selectionRect
  else []


view : Model -> Html Msg
view model =
  let 
    w = Maybe.map .width model.canvas |> Maybe.withDefault 1000 |> String.fromInt
    h = Maybe.map .height model.canvas |> Maybe.withDefault 1000 |> String.fromInt
  in
  div [HAttrs.id "annotation_overlay"] 
    [ svg [ SAttrs.viewBox ("0 0 " ++ w ++ " " ++ h) ] <| 
        (List.concatMap annotationSvg model.annotations) ++ (selectionOverlay model)
--    , div [ HAttrs.attribute "style" "position: absolute; left: 0; top: 0; width: 100%; height: 100%;"] [Html.text "Moomoo"]
    ]
