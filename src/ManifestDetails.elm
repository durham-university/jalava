module ManifestDetails exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Dict

import Element exposing(Element)
import UI.DefinitionList as DefinitionList

import Utils
import Iiif.Types exposing(..)


manifestDetails : Manifest -> Element msg
manifestDetails manifest = manifestDetailsExtras manifest []

manifestDetailsExtras : Manifest -> List (String, Maybe (Element msg)) -> Element msg
manifestDetailsExtras manifest extraValues =
  let 
    propertyItem : String -> Maybe (Element msg) -> List (DefinitionList.DefinitionListItem msg)
    propertyItem label maybeValue = 
      Maybe.map2 DefinitionList.DefinitionListItem (Just label) maybeValue
      |> Maybe.map List.singleton
      |> Maybe.withDefault []

    propertyItemText : String -> Maybe String -> List (DefinitionList.DefinitionListItem msg)
    propertyItemText label maybeValue = 
      let sanitised = Maybe.map (Element.html << (Html.div []) << Utils.sanitiseHtml) maybeValue
      in propertyItem label sanitised

    propertyItemLink : String -> String -> Maybe String -> List (DefinitionList.DefinitionListItem msg)
    propertyItemLink label valueLabel maybeValueUrl =
      maybeValueUrl
        |> Maybe.map (\valueUrl -> propertyItem label (Just <| Element.link [] {url = valueUrl, label = Element.text valueLabel}))
        |> Maybe.withDefault []


    multiFolder label value list = list ++ (propertyItemText label (Just value))

    metadataFolder label value list = List.foldl (multiFolder label) list value

    manifestLinkItem : String -> Maybe ManifestLink -> List (DefinitionList.DefinitionListItem msg)
    manifestLinkItem label maybeLink = 
      case maybeLink of
        Nothing -> []
        Just link ->
          propertyItemLink label (Maybe.withDefault (link.id) link.label) (Just link.id)
    
    extras = List.foldl (\(l, v) acc -> acc ++ (propertyItem l v)) [] extraValues

    allItems = 
        propertyItemText "Title" manifest.label
        ++ propertyItemText "Attribution" manifest.attribution
        ++ propertyItemText "Description" manifest.description
        ++ Dict.foldl metadataFolder [] manifest.metadata
        ++ List.concatMap (\x -> manifestLinkItem "See also" (Just x)) manifest.seeAlso
        ++ List.concatMap (\x -> manifestLinkItem "Related" (Just x)) manifest.related
        ++ propertyItemLink "License" (Maybe.withDefault "License" manifest.license) manifest.license
        ++ extras
  in
  DefinitionList.empty
    |> DefinitionList.items allItems
    |> DefinitionList.definitionList
  
