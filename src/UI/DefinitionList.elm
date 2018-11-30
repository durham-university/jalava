module UI.DefinitionList exposing(..)

import UI.Fonts exposing(..)
import UI.Colors as Colors
import UI.ColorUtils as C
import UI.Core exposing(..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

type alias DefinitionListItem msg =
  { label : String
  , value : Html msg
  }

type alias DefinitionListConfig msg =
  { items : List (DefinitionListItem msg)
  , labelAttributes : List (Attribute msg)
  , valueAttributes : List (Attribute msg)
  , narrow : Bool
  }

empty : DefinitionListConfig msg
empty =
  { items = []
  , labelAttributes = []
  , valueAttributes = []
  , narrow = False
  }

items : List (DefinitionListItem msg) -> DefinitionListConfig msg -> DefinitionListConfig msg
items is config = { config | items = is }

labelAttributes : List (Attribute msg) -> DefinitionListConfig msg -> DefinitionListConfig msg
labelAttributes ls config = { config | labelAttributes = ls }

valueAttributes : List (Attribute msg) -> DefinitionListConfig msg -> DefinitionListConfig msg
valueAttributes vs config = { config | valueAttributes = vs }

narrow : Bool -> DefinitionListConfig msg -> DefinitionListConfig msg
narrow b config = { config | narrow = b }


label : DefinitionListConfig msg -> DefinitionListItem msg -> Html msg
label config item = el [cssPadding <| cssPx 5, Attributes.style "font-weight" "bold"] (text (item.label ++ ":"))

value : DefinitionListConfig msg -> DefinitionListItem msg -> Html msg
value config item = el [cssPadding <| cssPx 5] item.value


definitionList : DefinitionListConfig msg -> Html msg
definitionList config =
  if config.narrow then definitionListNarrow config
  else definitionListTable config

definitionListNarrow : DefinitionListConfig msg -> Html msg
definitionListNarrow config =
  let
    mapper = \item ->
      [ label config item
      , value config item
      ]
  in
    column 5 ([fullWidth] ++ textBody) <| List.concatMap mapper config.items


definitionListTable : DefinitionListConfig msg -> Html msg
definitionListTable config =
  let
    columns : List (GridColumn (DefinitionListItem msg) msg)
    columns =
      [ { header = none
        , attributes = config.labelAttributes
        , view = label config
        }
      , { header = none
        , attributes = config.valueAttributes
        , view = value config
        }
      ]
  in
    grid ([fullWidth] ++ textBody) columns config.items