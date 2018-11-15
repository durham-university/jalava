module ManifestDetails exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict

import Iiif.Types exposing(..)

manifestDetails : Manifest -> Html msg
manifestDetails manifest = manifestDetailsExtras manifest []

manifestDetailsExtras : Manifest -> List (String, Maybe (Html msg)) -> Html msg
manifestDetailsExtras manifest extraValues =
  let 
    propertyHtml : String -> Maybe (Html msg) -> List (Html msg)
    propertyHtml label maybeValue = 
      case maybeValue of
        Just value -> 
          [ dt [ class "col-md-3 text-truncate" ] [ text label ]
          , dd [ class "col-md-9" ] [ value ]
          ]
        Nothing -> []

    propertyHtmlText : String -> Maybe String -> List (Html msg)
    propertyHtmlText label maybeValue = propertyHtml label (Maybe.map text maybeValue)

    propertyHtmlLink : String -> String -> Maybe String -> List (Html msg)
    propertyHtmlLink label valueLabel maybeValueUrl =
      Maybe.withDefault [] <| Maybe.map
        (\valueUrl -> propertyHtml label (Just <| a [href valueUrl] [text valueLabel]))
        maybeValueUrl

    multiFolder label value list = list ++ (propertyHtmlText label (Just value))

    metadataFolder label value list = List.foldl (multiFolder label) list value

    manifestLinkHtml : String -> Maybe ManifestLink -> List (Html msg)
    manifestLinkHtml label maybeLink = 
      case maybeLink of
        Nothing -> []
        Just link ->
          propertyHtmlLink label (Maybe.withDefault (link.id) link.label) (Just link.id)
    
    extras = List.foldl (\(l, v) acc -> acc ++ (propertyHtml l v)) [] extraValues
  in
  dl [ class "row" ] (
       propertyHtmlText "Title" manifest.label
    ++ propertyHtmlText "Attribution" manifest.attribution
    ++ propertyHtmlText "Description" manifest.description
    ++ Dict.foldl metadataFolder [] manifest.metadata
    ++ List.concatMap (\x -> manifestLinkHtml "See also" (Just x)) manifest.seeAlso
    ++ List.concatMap (\x -> manifestLinkHtml "Related" (Just x)) manifest.related
    ++ propertyHtmlLink "License" (Maybe.withDefault "License" manifest.license) manifest.license
    ++ extras
  )
