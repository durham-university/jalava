module UriMapper exposing(UriMapper, fromRegexes, uriMapperDecoder, empty, backRefReplace)

import Regex exposing(..)
import List.Extra as ListE
import Json.Decode as Decode

type alias UriMapper =
  { inflate : String -> String
  , deflate : String -> String
  }

empty : UriMapper
empty = {inflate = \s -> "", deflate = \s -> ""}

backRefReplace : String -> Regex.Match -> String
backRefReplace replacement match =
  let
    backRefRe : Regex
    backRefRe = Maybe.withDefault Regex.never <| Regex.fromString "\\$(\\d+|\\$)"

    repl : Regex.Match -> String
    repl m =
      if m.match == "$$" then "$"
      else case m.submatches of
         -- we've matched for \\d+ so this should always be the first case and it 
         -- should always parse fine as Int
        [Just d] -> 
          let index = Maybe.withDefault 0 (String.toInt d)
          in Maybe.withDefault "" <| Maybe.withDefault Nothing <| ListE.getAt (index - 1) match.submatches
        _ -> ""
  in Regex.replace backRefRe repl replacement

fromRegexes : List (Regex, String) -> List (Regex, String) -> UriMapper
fromRegexes deflaters inflaters =
  let
    firstMatch : List (Regex, String) -> String -> Maybe (Regex, String)
    firstMatch regexes s = ListE.find (\(regex, repl) -> List.length (Regex.findAtMost 1 regex s) > 0) regexes

    inflate : String -> String
    inflate s = 
      (firstMatch inflaters s)
      |> Maybe.map (\(regex, repl) -> Regex.replace regex (backRefReplace repl) s)
      |> Maybe.withDefault ""

    deflate : String -> String
    deflate s =
      (firstMatch deflaters s)
      |> Maybe.map (\(regex, repl) -> Regex.replace regex (backRefReplace repl) s)
      |> Maybe.withDefault ""
  in
  {inflate = inflate, deflate = deflate}


uriMapperDecoder : Decode.Decoder UriMapper
uriMapperDecoder = 
  Decode.map2 fromRegexes
    (Decode.field "deflaters" (Decode.list regexPatternDecoder))
    (Decode.field "inflaters" (Decode.list regexPatternDecoder))

regexPatternDecoder : Decode.Decoder (Regex, String)
regexPatternDecoder =
  Decode.map2 (\x y -> (x, y))
    (Decode.field "regex" regexDecoder)
    (Decode.field "pattern" Decode.string)

regexDecoder : Decode.Decoder Regex
regexDecoder = 
  let
    regexParser s =
      case Regex.fromString s of
        Just re -> Decode.succeed re
        Nothing -> Decode.fail "Error compiling regular expression"
  in
  Decode.string |> Decode.andThen regexParser 

  