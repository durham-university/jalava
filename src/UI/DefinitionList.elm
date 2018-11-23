module UI.DefinitionList exposing(..)

import UI.Colors as Colors
import UI.ColorUtils as C

import UI.Fonts exposing(..)
import UI.Icon exposing(icon)

import Element exposing(..)
import Element.Border as Border
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input

type alias DefinitionListItem msg =
  { label : String
  , value : Element msg
  }

type alias DefinitionListConfig msg =
  { items : List (DefinitionListItem msg)
  , widths : (Length, Length)
  , narrow : Bool
  }

empty : DefinitionListConfig msg
empty =
  { items = []
  , widths = (shrink, fill)
  , narrow = False
  }

items : List (DefinitionListItem msg) -> DefinitionListConfig msg -> DefinitionListConfig msg
items is config = { config | items = is }

widths : Length -> Length -> DefinitionListConfig msg -> DefinitionListConfig msg
widths w1 w2 config = { config | widths = (w1, w2) }

narrow : Bool -> DefinitionListConfig msg -> DefinitionListConfig msg
narrow b config = { config | narrow = b }


label : DefinitionListConfig msg -> DefinitionListItem msg -> Element msg
label config item = Element.el [padding 5, Font.bold] (text (item.label ++ ":"))

value : DefinitionListConfig msg -> DefinitionListItem msg -> Element msg
value config item = Element.el [padding 5] item.value


definitionList : DefinitionListConfig msg -> Element msg
definitionList config =
  if config.narrow then definitionListNarrow config
  else definitionListTable config

definitionListNarrow : DefinitionListConfig msg -> Element msg
definitionListNarrow config =
  let
    mapper = \item ->
      [ label config item
      , value config item
      ]
  in
    column (textBody ++ [width fill]) <| List.concatMap mapper config.items


definitionListTable : DefinitionListConfig msg -> Element msg
definitionListTable config =
  let
    columns : List (Column (DefinitionListItem msg) msg)
    columns =
      [ { header = Element.none
        , width = Tuple.first config.widths
        , view = label config
        }
      , { header = Element.none
        , width = Tuple.second config.widths
        , view = value config
        }
      ]
  in
    table textBody {data = config.items, columns = columns}