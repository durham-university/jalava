module Utils exposing(..)

import Iiif exposing(Iiif)

import Html exposing(Html)
import Html.Attributes

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x


{-
take : Int -> List a -> List a
take n list =
  case (n, list) of
    (0, _) -> []
    (_, []) -> []
    (_, x :: xs) -> [x] ++ take (n - 1) xs
-}

updateWith :
  ({subModel | iiif : Iiif, errors : List String} -> {model | iiif : Iiif, errors : List String}) ->
  (subMsg -> msg) ->
  ({model | iiif : Iiif, errors : List String} -> {subModel | iiif : Iiif, errors : List String}) ->
  (subMsg -> {subModel | iiif : Iiif, errors : List String} -> ( {subModel | iiif : Iiif, errors : List String}, Cmd subMsg )) ->
  subMsg ->
  {model | iiif : Iiif, errors : List String} ->
  ({model | iiif : Iiif, errors : List String}, Cmd msg)
updateWith toModel toMsg toSubModel updater subMsg model =
  let 
    subModel = toSubModel model
    (updatedSubModel, subCmd) = updater subMsg {subModel | iiif = model.iiif, errors = []}
    combinedErrors = model.errors ++ updatedSubModel.errors
    updatedModel = toModel updatedSubModel
  in
    ( {updatedModel | iiif = updatedSubModel.iiif, errors = combinedErrors}, Cmd.map toMsg subCmd )



{-
updateChain :
  (model -> (model, Cmd msg)) ->
  (model -> (model, Cmd msg)) ->
  model ->
  (model, Cmd msg)
updateChain updater1 updater2 model =
  let
    (m1, c1) = updater1 model
    (m2, c2) = updater2 m1
  in
    (m2, Cmd.batch [c1, c2])
-}

iiifLink : Iiif.Uri -> Html msg
iiifLink uri =
  Html.a [ Html.Attributes.href uri, Html.Attributes.class "iiif_link" ] []


pluralise : Int -> String -> String -> String
pluralise count singular plural =
  case count of
    1 -> (String.fromInt count) ++ " " ++ singular
    _ -> (String.fromInt count) ++ " " ++ plural