module Iiif.ImageApi exposing(..)

import List.Extra as ListE

import Iiif.Types exposing(..)

type Region 
  = FullRegion
  | Square
  | Box Int Int Int Int

type Size
  = FullSize
  | Max
  | Exactly Int Int
  | FitW Int
  | FitH Int
  | FitBox Int Int

type Rotation
  = NoRotation

type Quality
  = Default
  | Gray

canvasThumbnailUrl : Size -> Canvas -> String
canvasThumbnailUrl size canvas =
  case canvas.thumbnail of
    Just thumbnail ->resourceUrl FullRegion size NoRotation Default thumbnail
    Nothing -> case canvas.images of
                  x :: xs -> annotationUrl size x
                  [] -> ""

annotationUrl : Size -> Annotation -> String
annotationUrl size annotation = annotationUrlFull FullRegion size NoRotation Default annotation

annotationUrlFull : Region -> Size -> Rotation -> Quality -> Annotation -> String
annotationUrlFull region size rotation quality annotation =
  resourceUrl region size rotation quality annotation.resource

resourceUrl : Region -> Size -> Rotation -> Quality -> Resource -> String
resourceUrl region size rotation quality resource =
  case resource.service of
    Nothing -> Maybe.withDefault "" resource.id
    Just service ->
      imageServiceUrl region size rotation quality "jpg" service

matchSizeToAvailable : Service -> Size -> Size
matchSizeToAvailable service size =
  if List.length service.sizes == 0 then size
  else
    let 
      sorted = List.sortBy Tuple.second service.sizes
      -- default shouldn't happen, already checked that list isn't empty
      max = (ListE.last sorted) |> Maybe.withDefault (0,0)
      matched = case size of
        FullSize -> max
        Max -> max
        Exactly w h -> ListE.find (\s -> Tuple.first s >= w && Tuple.second s >= h) sorted |> Maybe.withDefault max
        FitW w -> ListE.find (\s -> Tuple.first s >= w) sorted |> Maybe.withDefault max
        FitH h -> ListE.find (\s -> Tuple.second s >= h) sorted |> Maybe.withDefault max
        FitBox w h -> ListE.find (\s -> Tuple.first s >= w && Tuple.second s >= h) sorted |> Maybe.withDefault max
    in Exactly (Tuple.first matched) (Tuple.second matched)


imageServiceUrl : Region -> Size -> Rotation -> Quality -> String -> Service -> String
imageServiceUrl region size rotation quality format service =
  let
    regionString = case region of
      FullRegion -> "full"
      Square -> "square"
      Box x y w h -> String.join "," (List.map String.fromInt [x,y,w,h])
    sizeString = case (matchSizeToAvailable service size) of
      FullSize -> "full"
      Max -> "max"
      Exactly w h -> String.join "," (List.map String.fromInt [w,h])
      FitW w -> (String.fromInt w) ++ ","
      FitH h -> "," ++ (String.fromInt h)
      FitBox w h -> "!" ++ (String.join "," (List.map String.fromInt [w,h]))
    rotationString = case rotation of
      NoRotation -> "0"
    qualityString = case quality of
      Default -> "default"
      Gray -> "gray"
  in
    (String.join "/" [service.id, regionString, sizeString, rotationString, qualityString]) ++ "." ++ format


osdSourceImage : Annotation -> String
osdSourceImage annotation =
  case annotation.resource.service of
    -- TODO: Should check service profile
    Just service -> service.id ++ "/info.json"
    Nothing -> Maybe.withDefault "" annotation.resource.id

osdSource : Canvas -> Maybe String
osdSource canvas = 
  List.head canvas.images
  |> Maybe.map osdSourceImage
