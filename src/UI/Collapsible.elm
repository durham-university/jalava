module UI.Collapsible exposing(..)

import Html.Attributes
import Task
import Browser.Dom as Dom
import Element exposing(..)

import UI.Tween as Tween

import UI.Button as Button

type alias CollapsibleConfig msg=
  { id : String
  , content : Element msg
  , hideLabel : Element msg
  , showLabel : Element msg
  }

type State = Visible | Hidden | Opening | Collapsing

type Msg  = Collapse
          | Open
          | CollapseWithInfo (Result Dom.Error Dom.Element)
          | OpenWithInfo (Result Dom.Error Dom.Element)
          | TweenMsg Tween.Msg

type alias Model =
  { config : CollapsibleConfig Msg
  , tween : Tween.Model Float
  , state : State
  }

emptyConfig : String -> CollapsibleConfig msg
emptyConfig id = 
  { id = id 
  , content = Element.none
  , hideLabel = Element.text "Hide"
  , showLabel = Element.text "Show"
  }

content : Element Msg -> Model -> Model
content c ({config} as model) = 
  {model | config = {config | content = c } }

emptyModel : String -> Model
emptyModel id = 
  { config = emptyConfig id
  , tween = Tween.empty 0
  , state = Visible
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map TweenMsg (Tween.subscriptions model.tween)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Collapse -> (model, Task.attempt CollapseWithInfo (Dom.getElement model.config.id))
    CollapseWithInfo result -> 
      case (model.state, result) of
        (Hidden, _) -> (model, Cmd.none)
        (Collapsing, _) -> (model, Cmd.none )
        (_, Err _) -> (model, Cmd.none)
        (_, Ok elementInfo) -> 
          let
            tween = Tween.floatTween |> Tween.from elementInfo.element.height |> Tween.to 0 |> Tween.setTween model.tween
          in
          ({ model | tween = tween, state = Collapsing }, Cmd.none )
    Open -> (model, Task.attempt OpenWithInfo (Dom.getElement model.config.id))
    OpenWithInfo result ->
      case (model.state, result) of
        (Visible, _) -> (model, Cmd.none)
        (Opening, _) -> (model, Cmd.none)
        (_, Err err) -> 
          let _ = Debug.log "error" err
          in (model, Cmd.none)
        (_, Ok elementInfo) -> 
          let
            _ = Debug.log "opening" elementInfo.element.height
            tween = Tween.floatTween |> Tween.to elementInfo.element.height |> Tween.setTween model.tween
          in
          ({ model | tween = tween, state = Opening }, Cmd.none )
    TweenMsg tweenMsg -> 
      let
        tween = Tween.update tweenMsg model.tween
        state = case (model.state, Tween.isActive tween) of
          (Opening, False) -> Visible
          (Collapsing, False) -> Hidden
          (_, _) -> model.state
      in
        ({ model | state = state, tween = tween }, Cmd.none)

collapsible : Model -> Element Msg
collapsible model = 
  let
    idAttribute = htmlAttribute (Html.Attributes.attribute "id" model.config.id)
    height = Tween.currentValue model.tween
    heightAttribute = case model.state of
      Visible -> []
      Hidden -> [Element.height (Element.px 0)]
      _ -> [Element.height (Element.px (round height))]
  in
    Element.el ([clipY] ++ heightAttribute) <| Element.el [idAttribute] model.config.content

toggleButton : Model -> Element Msg
toggleButton model =
  let 
    (toggleMsg, label) = case model.state of
                              Opening -> (Collapse, model.config.hideLabel)
                              Visible -> (Collapse, model.config.hideLabel)
                              Collapsing -> (Open, model.config.showLabel)
                              Hidden -> (Open, model.config.showLabel)
    
  in
    Button.slimLink |> Button.content label |> Button.onPress toggleMsg |> Button.button