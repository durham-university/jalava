module UI.Core exposing(..)

import Html as Html exposing(Html, Attribute, div, text)
import Html.Attributes as Attributes
import Html.Keyed as Keyed

row : Int -> (List (Attribute msg)) -> (List (Html msg)) -> Html msg
row spacing attrs items = 
  let 
    spacingClass = [Attributes.class ("spacing" ++ (String.fromInt spacing))]
  in
  div ([Attributes.class "row"] ++ spacingClass ++ attrs) items

wrappedRow : Int -> (List (Attribute msg)) -> (List (Html msg)) -> Html msg
wrappedRow spacing attrs items = 
  let 
    spacingClass = [Attributes.class ("spacing" ++ (String.fromInt spacing))]
  in
  div ([Attributes.class "row", Attributes.class "wrap"] ++ spacingClass ++ attrs) items

column : Int -> (List (Attribute msg)) -> (List (Html msg)) -> Html msg
column spacing attrs items = 
  let 
    spacingClass = [Attributes.class ("spacing" ++ (String.fromInt spacing))]
  in
  div ([Attributes.class "column"] ++ spacingClass ++ attrs) items

wrappedColumn : Int -> (List (Attribute msg)) -> (List (Html msg)) -> Html msg
wrappedColumn spacing attrs items = 
  let 
    spacingClass = [Attributes.class ("spacing" ++ (String.fromInt spacing))]
  in
  div ([Attributes.class "column", Attributes.class "wrap"] ++ spacingClass ++ attrs) items


el : List (Attribute msg) -> Html msg -> Html msg
el attrs child = div ([Attributes.class "el"] ++ attrs) [child]

keyedEl : String -> List (Attribute msg) -> Html msg -> Html msg
keyedEl key attrs child = Keyed.node "div" ([Attributes.class "el"] ++ attrs) [(key, child)]

button : List (Attribute msg) -> Html msg -> Html msg
button attrs label = el (attrs ++ [Attributes.attribute "role" "button"]) label

none : Html msg
none = text ""

type alias GridColumn a msg =
  { header : Html msg
  , attributes : List (Attribute msg)
  , view : a -> Html msg
  }

grid : List (Attribute msg) -> List (GridColumn a msg) -> List a -> Html msg
grid attrs columns data =
  let
    gridColumn : Int -> GridColumn a msg -> Html msg
    gridColumn index column_ = 
      div ( [ Attributes.style "grid-row" "1"
            , Attributes.style "grid-column" (String.fromInt (index + 1))
            ] ++ column_.attributes) [column_.header]

    dataValue : Int -> a -> Int -> GridColumn a msg -> Html msg
    dataValue rowIndex value columnIndex column_ =
      div ( [ Attributes.style "grid-row" (String.fromInt (rowIndex + 2))
            , Attributes.style "grid-column" (String.fromInt (columnIndex + 1))
            ] ++ column_.attributes) [column_.view value]

    dataRow : Int -> a -> List (Html msg)
    dataRow rowIndex value =
      List.indexedMap (dataValue rowIndex value) columns

  in
  div ([Attributes.class "grid"] ++ attrs)
                <| (List.indexedMap gridColumn columns) ++
                    (List.concat <| List.indexedMap dataRow data)

fullWidth : Attribute msg
fullWidth = Attributes.class "fullWidth"

fullHeight : Attribute msg
fullHeight = Attributes.class "fullHeight"

cssPx : Int -> String
cssPx amount = (String.fromInt amount) ++ "px"

cssPnt : Int -> String
cssPnt amount = (String.fromInt amount) ++ "%"

cssNum : Int -> String
cssNum = String.fromInt

cssColor : String -> Attribute msg
cssColor s = Attributes.style "color" s

cssBackgroundColor : String -> Attribute msg
cssBackgroundColor s = Attributes.style "background-color" s

cssWidth : String -> Attribute msg
cssWidth amount = Attributes.style "width" amount

cssHeight : String -> Attribute msg
cssHeight amount = Attributes.style "height" amount

cssPadding : String -> Attribute msg
cssPadding amount = Attributes.style "padding" amount

cssPadding4 : String -> String -> String -> String -> Attribute msg
cssPadding4 top right bottom left = Attributes.style "padding" <| String.join " " [top, right, bottom, left]

cssBorderRadius : String -> Attribute msg
cssBorderRadius amount = Attributes.style "border-radius" amount

cssBorderRadius4 : String -> String -> String -> String -> Attribute msg
cssBorderRadius4 top right bottom left = Attributes.style "border-radius" <| String.join " " [top, right, bottom, left]

cssBorderWidth : String -> Attribute msg
cssBorderWidth amount = Attributes.style "border-width" amount

cssBorderWidth4 : String -> String -> String -> String -> Attribute msg
cssBorderWidth4 top right bottom left = Attributes.style "border-width" <| String.join " " [top, right, bottom, left]

cssBorderStyle : String -> Attribute msg
cssBorderStyle s = Attributes.style "border-style" s

cssBorderColor : String -> Attribute msg
cssBorderColor s = Attributes.style "border-color" s

