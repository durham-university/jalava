module Update exposing(..)

import Json.Decode as Decode

import Element

catCmd : Cmd msg -> Cmd msg -> Cmd msg
catCmd a b =
  if a == Cmd.none then
    b
  else
    if b == Cmd.none then
      a
    else
      Cmd.batch [a, b]


chain :
  ( modelA -> ( modelB, Cmd msg, List outMsg ) ) ->
  ( modelA, Cmd msg, List outMsg ) ->
  ( modelB, Cmd msg, List outMsg )
chain updater (model, cmd, out) = 
  let 
    (model2, cmd2, out2) = updater model
  in
    (model2, (catCmd cmd cmd2), out ++ out2)


maybeChain :
  ( a -> modelA -> ( modelA, Cmd msg, List outMsg) ) ->
  Maybe a ->
  ( modelA, Cmd msg, List outMsg ) ->
  ( modelA, Cmd msg, List outMsg )
maybeChain updater maybeValue =
  case maybeValue of
    Nothing -> identity
    Just v -> chain (updater v)

maybeChain2 :
  ( a -> b -> modelA -> (modelA, Cmd msg, List outMsg) ) ->
  Maybe a ->
  Maybe b ->
  ( modelA, Cmd msg, List outMsg) ->
  ( modelA, Cmd msg, List outMsg)
maybeChain2 updater maybeValueA maybeValueB =
  case (maybeValueA, maybeValueB) of
    (Just valueA, Just valueB) -> chain (updater valueA valueB)
    _ -> identity

chainIf :
  Bool ->
  ( model -> ( model, Cmd msg, List outMsg ) ) ->
  ( model, Cmd msg, List outMsg ) ->
  ( model, Cmd msg, List outMsg )
chainIf cond updater state =
  if cond then chain updater state
  else state


fold :
  ( a -> model -> ( model, Cmd msg, List outMsg ) ) ->
  List a ->
  ( model, Cmd msg, List outMsg) ->
  ( model, Cmd msg, List outMsg )
fold folder l (model, cmd, out) = 
  case l of
    [] -> (model, cmd, out)
    x :: xs ->
      let 
        (model2, cmd2, out2) = folder x model 
      in
        fold folder xs (model2, (catCmd cmd cmd2), out ++ out2)


fold2 :
  ( a -> model -> ( model, Cmd msg ) ) ->
  List a ->
  ( model, Cmd msg, List outMsg) ->
  ( model, Cmd msg, List outMsg )
fold2 folder =
  let 
    folder2 item model =
      let (m, c) = folder item model
      in (m, c, [])
  in fold folder2


foldOut :
  ( a -> model -> (List outMsg) ) ->
  List a ->
  ( model, Cmd msg, List outMsg) ->
  ( model, Cmd msg, List outMsg )
foldOut folder =
  let 
    folder2 item model =
      let out = folder item model
      in (model, Cmd.none, out)
  in fold folder2


mapModel :
  (modelA -> modelB) ->
  ( modelA, Cmd msg, List outMsg ) ->
  ( modelB, Cmd msg, List outMsg )
mapModel updater (model, cmd, out) = (updater model, cmd, out)


mapCmd :
  ( msgA -> msgB ) ->
  ( model, Cmd msgA, List outMsg ) ->
  ( model, Cmd msgB, List outMsg )
mapCmd mapper (model, cmd, out) = (model, Cmd.map mapper cmd, out)


chain2 :
  (modelA -> (modelB, Cmd msg)) ->
  ( modelA, Cmd msg, List outMsg ) ->
  ( modelB, Cmd msg, List outMsg )
chain2 updater (model, cmd, out) = 
  let
    (model2, cmd2) = updater model
  in
    (model2, (catCmd cmd cmd2), out)

addOut :
  List outMsg ->
  ( model, Cmd msg, List outMsg ) ->
  ( model, Cmd msg, List outMsg )
addOut out2 (model, cmd, out) = (model, cmd, out ++ out2)


mapOut : 
  ( outMsgA -> List outMsgB ) -> 
  ( model, Cmd msg, List outMsgA ) ->
  ( model, Cmd msg, List outMsgB )
mapOut mapper (model, cmd, out) = 
  (model, cmd, List.foldl ((++) << mapper) [] out)


evalOut :
  ( outMsgA -> model -> (model, Cmd msg, List outMsgB) ) ->
  ( model, Cmd msg, List outMsgA ) ->
  ( model, Cmd msg, List outMsgB )
evalOut evaluator (model, cmd, out) =
  case out of
    [] -> (model, cmd, [])
    x :: xs ->
      evalOut evaluator (model, cmd, xs)
      |> chain (evaluator x)


evalOut2 :
  ( outMsg -> model -> (model, Cmd msg) ) ->
  ( model, Cmd msg, List outMsg ) ->
  ( model, Cmd msg )
evalOut2 evaluator (model, cmd, out) =
  case out of
    [] -> (model, cmd)
    x :: xs ->
      (model, cmd, xs) 
        |> chain2 (evaluator x)
        |> evalOut2 evaluator


ignoreOut : ( model, Cmd msg, List outMsg ) -> ( model, Cmd msg )
ignoreOut (model, cmd, out) = (model, cmd)



type alias Component model msg outMsg =
  { init : Decode.Value -> ( model, Cmd msg, List outMsg )
  , emptyModel : model
  , update : msg -> model -> ( model, Cmd msg, List outMsg )
  , view : model -> Element.Element msg
  , subscriptions : model -> Sub msg
  }

type alias ComponentAdapter modelParent modelSub msgParent msgSub outMsgParent outMsgSub =
  { component : Component modelSub msgSub outMsgSub
  , unwrapModel : modelParent -> modelSub
  , wrapModel : modelParent -> modelSub -> modelParent
  , wrapMsg : msgSub -> msgParent
  , outEvaluator : outMsgSub -> modelParent -> (modelParent, Cmd msgParent, List outMsgParent)
  }

type alias ComponentMount modelParent modelSub msgParent msgSub outMsgParent outMsgSub =
  { component : Component modelSub msgSub outMsgSub
  , unwrap : modelParent -> modelSub
  , pipe : modelParent -> (modelSub, Cmd msgSub, List outMsgSub) -> (modelParent, Cmd msgParent, List outMsgParent)
  , updater : msgSub -> modelParent -> (modelParent, Cmd msgParent, List outMsgParent)
  , init : Decode.Value -> modelParent -> (modelParent, Cmd msgParent, List outMsgParent)
  , view : modelParent -> Element.Element msgParent
  , subscriptions : modelParent -> Sub msgParent
  }

subComponent :
  ComponentAdapter modelParent modelSub msgParent msgSub outMsgParent outMsgSub
  -> ComponentMount modelParent modelSub msgParent msgSub outMsgParent outMsgSub
subComponent s =
  let 
    pipe : modelParent -> (modelSub, Cmd msgSub, List outMsgSub) -> (modelParent, Cmd msgParent, List outMsgParent)
    pipe model = mapCmd s.wrapMsg >> mapModel (s.wrapModel model) >> evalOut s.outEvaluator

    updater : msgSub -> modelParent -> (modelParent, Cmd msgParent, List outMsgParent)
    updater msg model = s.component.update msg (s.unwrapModel model) |> pipe model

    init : Decode.Value -> modelParent -> (modelParent, Cmd msgParent, List outMsgParent)
    init flags m = s.component.init flags |> pipe m

    view : modelParent -> Element.Element msgParent
    view model = Element.map s.wrapMsg <| s.component.view (s.unwrapModel model)

    subscriptions : modelParent -> Sub msgParent
    subscriptions model = Sub.map s.wrapMsg <| s.component.subscriptions (s.unwrapModel model)
  in
  { component = s.component
  , unwrap = s.unwrapModel
  , pipe = pipe
  , updater = updater
  , init = init
  , view = view
  , subscriptions = subscriptions
  }

