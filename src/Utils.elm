module Utils exposing(..)

import Iiif exposing(Iiif)

import Html exposing(Html)
import Html.Attributes

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x


arrayRemove : Int -> List a -> List a
arrayRemove index list =
  case (index, list) of
    (_, []) -> []
    (0, x :: xs) -> xs
    (_, x :: xs) -> x :: (arrayRemove (index - 1) xs)


iiifLink : Iiif.Uri -> Html msg
iiifLink uri =
  Html.a [ Html.Attributes.href uri, Html.Attributes.class "iiif_link" ] []


pluralise : Int -> String -> String -> String
pluralise count singular plural =
  case count of
    1 -> (String.fromInt count) ++ " " ++ singular
    _ -> (String.fromInt count) ++ " " ++ plural


wrapKey : ( {a | id: String} -> b ) -> ( {a | id: String} -> ( String, b ) )
wrapKey f = \o -> (o.id, f o)
