module SelectionTypes exposing(..)

import Json.Decode as Decode
import Json.Encode as Encode

type alias Rect =
  { x : Float
  , y : Float
  , w : Float
  , h : Float
  }

type SelectionOverlayMsg  = SetSelectionEnabled Bool
                          | ClearSelection


encodeSelection : Maybe Rect -> Encode.Value
encodeSelection maybeRect =
  case maybeRect of
    Just rect -> encodeRect rect
    Nothing -> Encode.null


selectionDecoder : Decode.Decoder (Maybe Rect)
selectionDecoder = Decode.nullable decodeRect


encodeSelectionOverlayMsg : SelectionOverlayMsg -> Encode.Value
encodeSelectionOverlayMsg msg =
  case msg of
    SetSelectionEnabled v -> Encode.object [ ("type", Encode.string "SetSelectionEnabled"), ("value", Encode.bool v)]
    ClearSelection -> Encode.object [ ("type", Encode.string "ClearSelection")]

selectionOverlayMsgDecoder : Decode.Decoder SelectionOverlayMsg
selectionOverlayMsgDecoder =
  Decode.field "type" Decode.string
    |> Decode.andThen (\msgType -> 
                        case msgType of
                          "SetSelectionEnabled" ->
                            Decode.map SetSelectionEnabled <| Decode.field "value" Decode.bool
                          "ClearSelection" ->
                            Decode.succeed ClearSelection
                          _ -> Decode.fail <| "Unexpected SelectionMsg type " ++ msgType
                      )

decodeRect : Decode.Decoder Rect
decodeRect =
    Decode.map4 Rect
      (Decode.field "x" Decode.float)
      (Decode.field "y" Decode.float)
      (Decode.field "w" Decode.float)
      (Decode.field "h" Decode.float)

encodeRect : Rect -> Encode.Value
encodeRect rect =
    Encode.object
      [ ("x", Encode.float rect.x)
      , ("y", Encode.float rect.y)
      , ("w", Encode.float rect.w)
      , ("h", Encode.float rect.h)
      ]