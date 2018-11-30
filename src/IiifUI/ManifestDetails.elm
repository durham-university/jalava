module IiifUI.ManifestDetails exposing(..)

import Dict

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import UI.Core exposing(..)
import UI.DefinitionList as DefinitionList

import IiifUI.IiifLink as IiifLink

import Utils

import Iiif.Types exposing(..)

type alias ManifestDetailsConfig =
  { manifest : Maybe Manifest
  , includeIiifLink : Bool
  }

empty : ManifestDetailsConfig
empty =
  { manifest = Nothing
  , includeIiifLink = False
  }

manifest : Manifest -> ManifestDetailsConfig -> ManifestDetailsConfig
manifest manifest_ config = {config | manifest = Just manifest_ }

maybeManifest : Maybe Manifest -> ManifestDetailsConfig -> ManifestDetailsConfig
maybeManifest maybeManifest_ config = {config | manifest = maybeManifest_ }

includeIiifLink : ManifestDetailsConfig -> ManifestDetailsConfig
includeIiifLink config = {config | includeIiifLink = True}

manifestDetails : ManifestDetailsConfig -> Html msg
manifestDetails config =
  case config.manifest of
    Nothing -> none
    Just manifest_ ->
      let 
        propertyItem : String -> Maybe (Html msg) -> Maybe (DefinitionList.DefinitionListItem msg)
        propertyItem label maybeValue = 
          Maybe.map2 DefinitionList.DefinitionListItem (Just label) maybeValue

        propertyItemText : String -> Maybe String -> Maybe (DefinitionList.DefinitionListItem msg)
        propertyItemText label maybeValue = 
          let sanitised = Maybe.map ((div [Attributes.class "formattedContent"]) << Utils.sanitiseHtml) maybeValue
          in propertyItem label sanitised

        propertyItemLink : String -> String -> Maybe String -> Maybe (DefinitionList.DefinitionListItem msg)
        propertyItemLink label valueLabel maybeValueUrl =
          maybeValueUrl
            |> Maybe.andThen (\valueUrl -> propertyItem label (Just <| a [Attributes.href valueUrl] <| [text valueLabel]))


        multiFolder label value list = list ++ [propertyItemText label (Just value)]

        metadataFolder label value list = List.foldl (multiFolder label) list value

        manifestLinkItem : String -> Maybe ManifestLink -> Maybe (DefinitionList.DefinitionListItem msg)
        manifestLinkItem label maybeLink = 
          Maybe.andThen (\link -> propertyItemLink label (Maybe.withDefault (link.id) link.label) (Just link.id)) maybeLink
        
        iiifLink = 
          if config.includeIiifLink then Just <| DefinitionList.DefinitionListItem "IIIF" (IiifLink.iiifLink manifest_.id)
          else Nothing
        
        allItems = List.filterMap identity <|
            [ propertyItemText "Title" manifest_.label
            , propertyItemText "Attribution" manifest_.attribution
            , propertyItemText "Description" manifest_.description
            ]  
            ++ Dict.foldl metadataFolder [] manifest_.metadata
            ++ List.map (\x -> manifestLinkItem "See also" (Just x)) manifest_.seeAlso
            ++ List.map (\x -> manifestLinkItem "Related" (Just x)) manifest_.related
            ++ 
            [ propertyItemLink "License" (Maybe.withDefault "License" manifest_.license) manifest_.license
            , iiifLink
            ]
      in
      DefinitionList.empty
        |> DefinitionList.items allItems
        |> DefinitionList.definitionList
  
