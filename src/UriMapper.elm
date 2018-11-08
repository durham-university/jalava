module UriMapper exposing(shortenUri, completeUri)

import Iiif exposing(Uri)

shortenUri : Uri -> String
shortenUri uri =
  let
    reverse = List.reverse <| String.split "/" uri
  in
    case reverse of
      [] -> ""
      "manifest" :: id :: xs -> id
      id :: xs -> id

completeUri : String -> Uri
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
