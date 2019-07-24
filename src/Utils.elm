module Utils exposing(..)

import Iiif.Types

import Html as Html exposing(Html) 
import Html.Attributes as Attributes
import Regex

import XmlParser exposing (..)
import VirtualDom as Dom

type ScrollAxis = ScrollX | ScrollY

type ScrollAlignment = ScrollStart | ScrollMiddle

type alias ScrollInfo = 
  { containerId : String
  , targetId : String
  , axis : ScrollAxis
  , animate : Bool
  , alignment : ScrollAlignment
  }

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

arrayRemove : Int -> List a -> List a
arrayRemove index list =
  case (index, list) of
    (_, []) -> []
    (0, x :: xs) -> xs
    (_, x :: xs) -> x :: (arrayRemove (index - 1) xs)


pluralise : Int -> String -> String -> String
pluralise count singular plural =
  case count of
    1 -> (String.fromInt count) ++ " " ++ singular
    _ -> (String.fromInt count) ++ " " ++ plural


wrapKey : ( {a | id: String} -> b ) -> ( {a | id: String} -> ( String, b ) )
wrapKey f = \o -> (o.id, f o)


sanitiseId : String -> String
sanitiseId s = 
  let 
    re = Maybe.withDefault Regex.never (Regex.fromString "[^a-z0-9]+")
  in
    Regex.replace re (\_ -> "_") s

sanitiseHtml : String -> List (Html msg)
sanitiseHtml string =
  let
    allowedTags = ["div", "span", "a", "img", "ol", "ul", "dl", "dd", "dt", "table", "tbody", "thead", "tfoot", "tr", "td", "th", "li", "p", "i", "u", "em", "small", "b", "sub", "sup", "abbr", "cite", "del", "ins", "pre", "br", "h1", "h2", "h3", "h4", "h5", "h6", "hr"]
    allowedAttrs = ["href", "src", "style", "class", "title"]


    sanitiseNode : XmlParser.Node -> Html msg
    sanitiseNode node =
      case node of
        XmlParser.Element elem attributes children -> 
          if List.member elem allowedTags then
            let 
              sanitisedAttributes =
                attributes 
                  |> List.filter (\a -> List.member a.name allowedAttrs) 
                  |> List.map (\a -> Attributes.attribute a.name a.value)
            in
              Html.node elem sanitisedAttributes (List.map sanitiseNode children)
          else Html.text ""
        XmlParser.Text text -> Html.text text
    
    parsed = XmlParser.parse ("<root>" ++ string ++ "</root>")
  in 
    case parsed of 
      Ok xml -> 
        case xml.root of
          XmlParser.Element "root" attributes children ->
            List.map sanitiseNode children
          _ -> [Html.text ""]
      Err error -> [Html.text ""]