module IiifUI.CollectionDetails exposing(..)

import Dict

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import UI.Core exposing(..)
import UI.DefinitionList as DefinitionList

import IiifUI.IiifLink as IiifLink

import Utils

import Iiif.Types exposing(..)

type alias CollectionDetailsConfig =
  { collection : Maybe Collection
  , includeIiifLink : Bool
  }

empty : CollectionDetailsConfig
empty =
  { collection = Nothing
  , includeIiifLink = False
  }

collection : Collection -> CollectionDetailsConfig -> CollectionDetailsConfig
collection collection_ config = {config | collection = Just collection_ }

maybeCollection : Maybe Collection -> CollectionDetailsConfig -> CollectionDetailsConfig
maybeCollection maybeCollection_ config = {config | collection = maybeCollection_ }

includeIiifLink : CollectionDetailsConfig -> CollectionDetailsConfig
includeIiifLink config = {config | includeIiifLink = True}

collectionDetails : CollectionDetailsConfig -> Html msg
collectionDetails config =
  case config.collection of
    Nothing -> none
    Just collection_ ->
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

--        collectionLinkItem : String -> Maybe CollectionLink -> Maybe (DefinitionList.DefinitionListItem msg)
--        collectionLinkItem label maybeLink = 
--          Maybe.andThen (\link -> propertyItemLink label (Maybe.withDefault (link.id) link.label) (Just link.id)) maybeLink
        
        iiifLink = 
          if config.includeIiifLink then Just <| DefinitionList.DefinitionListItem "IIIF" (IiifLink.iiifLink collection_.id)
          else Nothing
        
        allItems = List.filterMap identity <|
            [ propertyItemText "Title" collection_.label
--            , propertyItemText "Attribution" collection_.attribution
            , propertyItemText "Description" collection_.description
            ]  
--            ++ Dict.foldl metadataFolder [] collection_.metadata
--            ++ List.map (\x -> collectionLinkItem "See also" (Just x)) collection_.seeAlso
--            ++ List.map (\x -> collectionLinkItem "Related" (Just x)) collection_.related
            ++ 
--            [ propertyItemLink "License" (Maybe.withDefault "License" collection_.license) collection_.license
            [ iiifLink
            ]
      in
      DefinitionList.empty
        |> DefinitionList.items allItems
        |> DefinitionList.definitionList
  
