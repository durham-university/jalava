module UI.Collapsible exposing(..)

import Process
import Task
import Browser.Events
import Browser.Dom as Dom
import Json.Decode as Decode
import Time
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

import UI.Core exposing(..)

import Update as U
import UI.Button as Button

type alias CollapsibleConfig msg=
  { id : Maybe String
  , content : Html msg
  , hideLabel : Html msg
  , showLabel : Html msg
  , collapseTime : Float
  , attributes : List (Attribute msg)
  }

type State = Visible | CollapseStart | Collapsing | Hidden | OpenPreStart | OpenStart | Opening

type Msg  = Collapse
          | CollapseInfo (Result Dom.Error Dom.Element)
          | CollapseFrame Time.Posix
          | CollapseDone
          | Open
          | OpenInfo (Result Dom.Error Dom.Element)
          | OpenPreFrame Time.Posix
          | OpenFrame Time.Posix
          | OpenDone

type OutMsg = Never

type alias Model =
  { config : CollapsibleConfig Msg
  , collapseHeight : Float
  , state : State
  }

component : U.Component Model Msg OutMsg
component = { init = init, emptyModel = emptyModel, update = update, view = view, subscriptions = subscriptions }

init : Decode.Value -> ( Model, Cmd Msg, List OutMsg )
init flags = (emptyModel, Cmd.none, [])

emptyConfig : CollapsibleConfig msg
emptyConfig = 
  { id = Nothing
  , content = none
  , hideLabel = text "Hide"
  , showLabel = text "Show"
  , collapseTime = 200
  , attributes = []
  }

content : Html Msg -> Model -> Model
content c ({config} as model) = 
  {model | config = {config | content = c } }

emptyModel : Model
emptyModel  = 
  { config = emptyConfig
  , collapseHeight = 0
  , state = Visible
  }

attributes : List (Attribute Msg) -> Model -> Model
attributes attrs ({config} as model) = {model | config = {config | attributes = config.attributes ++ attrs} }

labels : Html Msg -> Html Msg -> Model -> Model
labels showLabel hideLabel ({config} as model) =
  {model | config = {config | showLabel = showLabel, hideLabel = hideLabel }}

id : String -> Model -> Model
id id_ ({config} as model) = {model | config = {config | id = Just id_} }

closed : Model -> Model
closed model = {model | state = Hidden}

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    CollapseStart -> Browser.Events.onAnimationFrame CollapseFrame
    OpenPreStart -> Browser.Events.onAnimationFrame OpenPreFrame
    OpenStart -> Browser.Events.onAnimationFrame OpenFrame
    _ -> Sub.none

-- TODO: autogenerate id if not set

update : Msg -> Model -> (Model, Cmd Msg, List OutMsg)
update msg model = 
  case msg of
    Collapse -> (model, Task.attempt CollapseInfo (Dom.getElement <| Maybe.withDefault "" model.config.id), [])
    CollapseInfo result -> 
      case (model.state, result) of
        (Hidden, _) -> (model, Cmd.none, [])
        (Collapsing, _) -> (model, Cmd.none, [])
        (_, Err err) -> 
          let _ = Debug.log "Collapsible error" err
          in (model, Cmd.none, [])
        (_, Ok elementInfo) -> 
          ({ model | state = CollapseStart, collapseHeight = elementInfo.element.height}, Cmd.none, [])
    CollapseFrame _ -> ({ model | state = Collapsing }, Task.perform (\_ -> CollapseDone) (Process.sleep model.config.collapseTime), [])
    CollapseDone -> ({model | state = Hidden}, Cmd.none, [])
    Open -> ({model | state = OpenPreStart}, Cmd.none, [])
    OpenPreFrame _ -> (model, Task.attempt OpenInfo (Dom.getElement <| Maybe.withDefault "" model.config.id), [])
    OpenInfo result ->
      case (model.state, result) of
        (Visible, _) -> (model, Cmd.none, [])
        (Opening, _) -> (model, Cmd.none, [])
        (_, Err err) -> 
          let _ = Debug.log "Collapsible error" err
          in (model, Cmd.none, [])
        (_, Ok elementInfo) -> 
          ({ model | state = OpenStart, collapseHeight = elementInfo.element.height}, Cmd.none, [])
    OpenFrame _ -> ({ model | state = Opening }, Task.perform (\_ -> OpenDone) (Process.sleep model.config.collapseTime), [])
    OpenDone -> ({model | state = Visible}, Cmd.none, [])

view : Model -> Html Msg
view model = 
  let
    idAttribute = Attributes.attribute "id" <| Maybe.withDefault "" model.config.id
    heightAttribute = case model.state of
      Visible -> []
      CollapseStart -> [cssHeight (cssPx <| round model.collapseHeight)]
      Collapsing -> [cssHeight (cssPx 0)]
      Hidden -> [cssHeight (cssPx 0), Attributes.class "hide"]
      OpenPreStart -> [cssHeight (cssPx 0)]
      OpenStart -> [cssHeight (cssPx 0)]
      Opening -> [cssHeight (cssPx <| round model.collapseHeight)]
--      _ -> [Css.height (px height)]
  in
    el ( [ Attributes.style "overflow-y" "hidden"
         , Attributes.style "transition" ("height " ++ (String.fromFloat model.config.collapseTime) ++ "ms")
         ] ++ heightAttribute ++ model.config.attributes )
        <| el [idAttribute, Attributes.style "flex-shrink" "0", fullWidth, fullHeight] model.config.content

toggleButtonInfo : Model -> (Msg, Html Msg)
toggleButtonInfo model =
  case model.state of
    OpenPreStart -> (Collapse, model.config.hideLabel)
    OpenStart -> (Collapse, model.config.hideLabel)
    Opening -> (Collapse, model.config.hideLabel)
    Visible -> (Collapse, model.config.hideLabel)
    CollapseStart -> (Open, model.config.showLabel)
    Collapsing -> (Open, model.config.showLabel)
    Hidden -> (Open, model.config.showLabel)

toggleButton : Model -> Html Msg
toggleButton model =
  let 
    (toggleMsg, label) = toggleButtonInfo model
  in
    Button.slimLink |> Button.content label |> Button.onPress toggleMsg |> Button.button