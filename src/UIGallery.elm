port module UIGallery exposing(..)

import Browser
import Browser.Navigation as Nav

import Url
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing(Set)
import Dict exposing(Dict)

import Iiif.Types exposing(..)
import Iiif.Stubs exposing(..)
import Iiif.Utils
import Iiif.InternalUtils

import Update as U

import UI.Core exposing(..)
import UI.Button as Button
import UI.Panel as Panel
import UI.Toast as Toast
import UI.DefinitionList as DefinitionList
import UI.Icon as Icon
import UI.TitleLine as TitleLine
import UI.Tree as Tree
import UI.Tabs as Tabs
import UI.Colors as Colors
import UI.Collapsible as Collapsible

import Html as Html exposing (..)
import Html.Attributes as Attributes

import IiifUI.ManifestDetails as ManifestDetails
import IiifUI.ManifestPanel as ManifestPanel

import UI.ColorUtils as C

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , selectedTab : Int
  , collapsible : Collapsible.Model
  , manifestPanel : ManifestPanel.Model
  }

type Msg = LinkClicked Browser.UrlRequest
         | UrlChanged Url.Url
         | SelectTab Int
         | CollapsibleMsg Collapsible.Msg
         | ManifestPanelMsg ManifestPanel.Msg

collapsible = 
  U.subComponent 
    { component = Collapsible.component 
    , unwrapModel = .collapsible
    , wrapModel = \model subModel -> { model | collapsible = subModel }
    , wrapMsg = CollapsibleMsg
    , outEvaluator = \msgSub model -> (model, Cmd.none, [])
    }

main : Program Decode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  parseUrl url (emptyModel url key)


emptyModel : Url.Url -> Nav.Key -> Model
emptyModel url key = 
  { key = key
  , url = url
  , selectedTab = 0
  , collapsible = Collapsible.emptyModel |> Collapsible.id "collapsible_id" |> Collapsible.content (UI.Core.el [cssPadding <| cssPx 15] textParagraphs)
  , manifestPanel = ManifestPanel.emptyModel |> ManifestPanel.id "manifestPanel_id" |> ManifestPanel.manifest testManifest
  }

parseUrl : Url.Url -> Model -> (Model, Cmd Msg)
parseUrl url model =
  let
    tab = 
      url.fragment
      |> Maybe.andThen String.toInt
      |> Maybe.withDefault 0
  in
    ({model | selectedTab = tab}, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url -> 
      if url == model.url then (model, Cmd.none)
      else parseUrl url model
    SelectTab tab -> 
      let 
        oldUrl = model.url
        newUrl = { oldUrl | fragment = Just <| String.fromInt tab }
      in
        ({model | selectedTab = tab}, Nav.pushUrl model.key (Url.toString newUrl))
    CollapsibleMsg collapsibleMsg ->
      (model, Cmd.none, []) |> U.chain (collapsible.updater collapsibleMsg) |> U.ignoreOut
    ManifestPanelMsg manifestPanelMsg ->
      let
        (newManifestPanel, manifestPanelCmd, outMsg) = ManifestPanel.update manifestPanelMsg model.manifestPanel
      in
        ({model | manifestPanel = newManifestPanel}, Cmd.map ManifestPanelMsg manifestPanelCmd)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch 
      [ Sub.map CollapsibleMsg (Collapsible.subscriptions model.collapsible)
      , Sub.map ManifestPanelMsg (ManifestPanel.subscriptions model.manifestPanel)
      ]



view : Model -> Browser.Document Msg
view model =
  { title = "Elm IIIF"
  , body = 
      [ UI.Core.column 10 [fullHeight, fullWidth]
          [ tabs model 
          , Maybe.withDefault UI.Core.none <| tabElement model model.selectedTab
          ]
      ]
  }

tabContent : List (Html Msg, Html Msg)
tabContent =
  [ (TitleLine.simple "Buttons", buttons)
  , (TitleLine.simple "Toasts", toasts)
  , (TitleLine.simple "Panels", panels)
  , (TitleLine.simple "DefinitionList", definitionList)
  , (TitleLine.simple "Tree", trees)
  , (TitleLine.simple "Collapsible", UI.Core.none)
  , (TitleLine.simple "Manifest Details", manifestDetails)
  , (TitleLine.simple "Manifest Panel", UI.Core.none)
  ]


tabElement : Model -> Int -> Maybe (Html Msg)
tabElement model index =
  let 
    finder ls i =
      if i == 0 then Maybe.map Tuple.second (List.head ls)
      else case ls of
        [] -> Nothing
        x :: xs -> finder xs (i - 1)
  in 
    if index == 5 then Just (collapsibleView model)
    else  if index == 7 then Just (manifestPanel model)
          else finder tabContent index


tabs : Model -> Html Msg
tabs model = 
  let
    ts = List.indexedMap (\ind (label, _) -> (ind, label)) tabContent
  in
  Tabs.default
  |> Tabs.content ts
  |> Tabs.selected model.selectedTab
  |> Tabs.onPress (\ind -> SelectTab ind)
  |> Tabs.tabs
  



toasts : Html msg
toasts =
  UI.Core.column 10 [fullHeight, fullWidth, cssPadding (cssPx 15)]
    [ Toast.primary |> Toast.content (TitleLine.simple "Primary") |> Toast.toast
    , Toast.secondary |> Toast.content (TitleLine.withIcon "bell" "Secondary") |> Toast.toast
    , Toast.error |> Toast.content (TitleLine.simple "Error") |> Toast.toast
    , Toast.warning |> Toast.content (TitleLine.simple "Warning") |> Toast.toast
    , Toast.success |> Toast.content (TitleLine.simple "Success") |> Toast.toast
    , Toast.white |> Toast.content (TitleLine.simple "White") |> Toast.toast
    ]

buttons : Html msg
buttons = 
  UI.Core.row 10 [cssPadding (cssPx 15)]
    [ Button.primary |> Button.content (TitleLine.withIcon "save" "Primary") |> Button.button
    , Button.secondary |> Button.content (TitleLine.simple "Secondary") |> Button.button
    , Button.danger |> Button.content (TitleLine.withIcon "trash" "Danger") |> Button.button
    , Button.light |> Button.content (TitleLine.simple "Light") |> Button.button
    , Button.dark |> Button.content (TitleLine.simple "Dark") |> Button.button
    , Button.link |> Button.content (TitleLine.simple "Link") |> Button.button
    , Button.primary |> Button.content (TitleLine.iconOnly "search") |> Button.button
    ]


panels : Html msg
panels =
  UI.Core.wrappedRow 20 [cssPadding (cssPx 15)]
    [ Panel.default |> Panel.header (TitleLine.simple "Default panel") |> Panel.addSection (text "Short section") |> Panel.addSection textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth <| cssPx 500] |> Panel.panel
    , Panel.info |> Panel.header (TitleLine.simple "Info panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 500)] |> Panel.panel
    , Panel.error |> Panel.header (TitleLine.withIcon "ban" "Error panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    , Panel.warning |> Panel.header (TitleLine.simple "Warning panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    , Panel.success |> Panel.header (TitleLine.simple "Success panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    , Panel.success |> Panel.header (TitleLine.simple "All green panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.fullColor |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    , Panel.white |> Panel.header (TitleLine.simple "White panel") |> Panel.content textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 500)] |> Panel.panel
    , Panel.default |> Panel.content textParagraph |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    , Panel.default |> Panel.popup |> Panel.header (TitleLine.simple "Popup panel") |> Panel.addSection textParagraph |> Panel.footer (text "Panel footer") |> Panel.attributes [cssWidth (cssPx 300)] |> Panel.panel
    ]


definitionList : Html Msg
definitionList =
  UI.Core.el [fullWidth, cssPadding (cssPx 15)] (
    DefinitionList.empty |> DefinitionList.items 
    [ {label = "Test label", value = text "Test value"}
    , {label = "Another test label", value = text "Another test value"}
    , {label = "Longer value", value = textParagraphs}
    ] |> DefinitionList.definitionList
  )

type TreeNode = Node String (List TreeNode)

tree : Tree.TreeConfig TreeNode msg -> Html msg
tree treeConfig = 
  let
    roots =
      [ Node "Tree 1"
        [ Node "Item 1" 
          [ Node "Item A" []
          , Node "Item B" []
          , Node "Item C" []
          , Node "Item D" []
          ]
        , Node "Item 2" []
        ]
      , Node "Tree 2"
        [ Node "Item 3" []
        , Node "Item 4" []
        ]
      ]
    label n = case n of
      Node label_ children_ -> TitleLine.simple label_
    children n = case n of
      Node label_ children_ -> children_
    selected n = case n of
      Node label_ children_ -> label_ == "Item B"
  in 
    treeConfig
      |> Tree.attributes [cssWidth (cssPx 200)]
      |> Tree.rootItems roots
      |> Tree.label label
      |> Tree.children children
      |> Tree.selected selected
      |> Tree.tree

trees : Html msg
trees =
  UI.Core.row 30 [cssPadding (cssPx 15)]
    [ tree (Tree.empty |> Tree.icon (\_ -> Just <| Icon.icon "folder" []))
    , tree (Tree.empty |> Tree.color "Secondary" )
--    , tree (Tree.empty |> Tree.color Colors.defaultTextColor |> Tree.selectColor Colors.primary )
    ]

textParagraphs : Html msg
textParagraphs = 
  UI.Core.column 10 []
    [ textParagraph, textParagraph ]

textParagraph : Html msg
textParagraph = p [] [text loremIpsum]

loremIpsum : String
loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam iaculis suscipit arcu ut accumsan. Aenean nec dolor aliquam dui tristique malesuada egestas eget risus. Vivamus dapibus nibh ut orci pretium mollis. Fusce lacinia eros id libero hendrerit volutpat. Nullam lobortis lacus a ultricies pulvinar. Cras placerat egestas porttitor."

collapsibleView : Model -> Html Msg
collapsibleView model =
  Panel.default |> Panel.header (TitleLine.simple "Default panel") 
                |> Panel.addDirectSection (Collapsible.view model.collapsible)
                |> Panel.footer (Collapsible.toggleButton model.collapsible)
                |> Panel.attributes [cssWidth (cssPx 500)] |> Panel.panel |> Html.map CollapsibleMsg
                |> UI.Core.el [cssPadding (cssPx 15)]


manifestDetails : Html msg
manifestDetails =
  ManifestDetails.empty
    |> ManifestDetails.manifest testManifest
    |> ManifestDetails.includeIiifLink
    |> ManifestDetails.manifestDetails
    |> UI.Core.el [cssPadding (cssPx 15)]


manifestPanel : Model -> Html Msg
manifestPanel model =
  model.manifestPanel |> ManifestPanel.view |> Html.map ManifestPanelMsg
    |> UI.Core.el [cssPadding (cssPx 15), cssWidth <| cssPx 500]

testIiif : Iiif
testIiif = Iiif.Utils.empty |> Iiif.InternalUtils.addManifest testManifest

testManifest : Manifest
testManifest =
  let
    thumbnail : Resource
    thumbnail =
      { id = Just "test_canvas_small.jpg"
      , resourceType = Nothing
      , format = Just "image/jpeg"
      , width = Just 54
      , height = Just 60
      , service = Nothing
      , chars = Nothing
      , label = Nothing
      }

    canvas : Canvas
    canvas = 
      { id = "http://www.example.com/canvas"
      , label = Just "test"
      , width = 54
      , height = 60
      , images = []
      , thumbnail = Just thumbnail
      , otherContent = []
      }

    sequence : Sequence
    sequence =  
      { id = Nothing
      , label = Just "default"
      , viewingDirection = Nothing
      , viewingHint = Nothing
      , canvases = [ canvas, canvas, canvas, canvas, canvas, canvas ]
      }
  in
    stubManifest "http://www.example.com" (Just "Test manifest") (Just "test_manifest_logo.png")
      |> \x -> {x | description = Just ("<b>Test formatting</b> " ++ loremIpsum)
                  , license = Just "All rights reserved"
                  , attribution = Just "Test attribution"
                  , metadata = Dict.fromList [("Keywords", ["Test", "UI"]), ("Test metadata", ["Test metadata value"])]
                  , related = [ManifestLink "http://www.example.com/related" (Just "Related") Nothing Nothing]
                  , seeAlso = [ManifestLink "http://www.example.com/seeAlsa" (Just "See also") Nothing Nothing]
                  , status = Full
                  , sequences = [ sequence ]
               }