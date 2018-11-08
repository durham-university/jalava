module Config exposing(..)

import Iiif

miradorBase = "https://iiif.durham.ac.uk/index.html"

manifestViewerUrl : Iiif.Manifest -> String
manifestViewerUrl manifest = 
  let shortId = shortenUri manifest.id
  in miradorBase ++ "?manifest=" ++ shortId

canvasViewerUrl : Iiif.Manifest -> Iiif.Canvas -> String
canvasViewerUrl manifest canvas =
  let 
    manifestUrl = manifestViewerUrl manifest
    shortId = shortenUri canvas.id
  in manifestUrl ++ "&canvas=" ++ shortId

shortenUri : Iiif.Uri -> String
shortenUri uri =
  let
    reverse = List.reverse <| String.split "/" uri
  in
    case reverse of
      [] -> ""
      "manifest" :: id :: xs -> id
      id :: xs -> id

completeUri : String -> Iiif.Uri
completeUri id =
  let
    chars = String.split "" id
  in
    case List.take 1 <| List.drop 2 chars of
      ["m"] -> 
        let 
          pairtree = String.join "/" 
            [ String.join "" <| List.take 2 chars
            , String.join "" <| List.take 2 <| List.drop 2 chars
            , String.join "" <| List.take 2 <| List.drop 4 chars
            ]
        in
          "https://iiif.durham.ac.uk/manifests/trifle/32150/" ++ pairtree ++"/" ++ id ++ "/manifest"
      _ -> "https://iiif.durham.ac.uk/manifests/trifle/collection/32150/" ++ id
