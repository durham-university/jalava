module Update exposing(..)

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
