module UI.Tween exposing(..)

import Time
import Task
import Browser.Events as Events

type alias Tween a =
  { from : Maybe a
  , to : a
  , duration : TimeMillis
  , interpolation : Float -> a -> a -> a
  } 

type alias ActiveTween a =
  { tween : Tween a
  , startTime : TimeMillis
  , from : a
  }

type alias Model a =
  { activeTween : Maybe (ActiveTween a)
  , queue : List (Tween a)
  , currentValue : a
  }

type Msg = Tick Time.Posix

type alias TimeMillis = Int

empty : a -> Model a
empty value =
  { activeTween = Nothing
  , queue = []
  , currentValue = value
  }

tween : a -> Tween a
tween to_ = 
  { from = Nothing
  , to = to_
  , duration = 200
  , interpolation = \_ _ x -> x
  }

floatTween : Tween Float
floatTween =
  { from = Nothing
  , to = 0.0
  , duration = 200
  , interpolation = easeInQuad
  }

from : a -> Tween a -> Tween a
from from_ tween_ = {tween_ | from = Just from_ }

to : a -> Tween a -> Tween a
to to_ tween_ = {tween_ | to = to_ }

interpolation : (Float -> a -> a -> a) -> Tween a -> Tween a
interpolation int_ tween_ = {tween_ | interpolation = int_}

duration : TimeMillis -> Tween a -> Tween a
duration d tween_ = {tween_ | duration = d}

init : a -> (Model a, Cmd msg)
init value = (empty value, Cmd.none)

currentValue : Model a -> a
currentValue model = model.currentValue

isActive : Model a -> Bool
isActive model =
  case model.activeTween of
    Just _ -> True
    Nothing -> False

subscriptions : Model a -> Sub Msg
subscriptions model = 
  case (model.activeTween, model.queue) of
    (Just _, _) -> Events.onAnimationFrame Tick
    (_, x :: xs) -> Events.onAnimationFrame Tick
    _ -> Sub.none

update : Msg -> Model a -> Model a
update msg model =
  case msg of 
    Tick time -> 
      let
        millis = Time.posixToMillis time
        newModel = model |> processActive millis |> processQueue millis
      in
        newModel

queueTween : Model a -> Tween a -> Model a
queueTween model tween_ =
  {model | queue = model.queue ++ [tween_] }

setTween : Model a -> Tween a -> Model a
setTween model tween_ = 
  {model | activeTween = Nothing, queue = [tween_] }

processActive : TimeMillis -> Model a -> Model a
processActive time model =
  case model.activeTween of
    Just active ->
      let
        endTime = active.startTime + active.tween.duration
        clampedTime = min time endTime
        internal = toFloat (clampedTime - active.startTime) / toFloat active.tween.duration
        value = active.tween.interpolation internal active.from active.tween.to
        newActive = if clampedTime == endTime then Nothing
                    else Just active
      in
        { model | activeTween = newActive, currentValue = value }
    Nothing -> model

processQueue : TimeMillis -> Model a -> Model a
processQueue time model =
  case (model.activeTween, model.queue) of
    (Nothing, t :: ts) ->
      let
        nextTween = 
          { tween = t
          , startTime = time
          , from = case t.from of
                      Just f -> f
                      Nothing -> model.currentValue
          }
      in
        { model | activeTween = Just nextTween, queue = ts, currentValue = nextTween.from }
    _ -> model

linear : Float -> Float -> Float -> Float
linear internal from_ to_ = from_ + (to_ - from_) * internal

easeInQuad : Float -> Float -> Float -> Float
easeInQuad internal from_ to_ = from_ + (to_ - from_) * internal * internal

easeOutQuad : Float -> Float -> Float -> Float
easeOutQuad internal from_ to_ = from_ + (from_ - to_) * internal * (internal - 2.0)

easeInOutQuad : Float -> Float -> Float -> Float
easeInOutQuad internal from_ to_ = 
  let half = (from_ + (to_ - from_) / 2.0)
  in
    if internal < 0.5 then easeInQuad (internal * 2.0) from_ half
    else easeOutQuad (internal * 2.0 - 1.0) half to_

timeCompress : Float -> Float -> (Float -> a -> a -> a) -> Float -> a -> a -> a
timeCompress startTime endTime interpolator internal from_ to_ =
  if internal < startTime then interpolator startTime from_ to_
  else if internal > endTime then interpolator endTime from_ to_
      else interpolator (internal - startTime / (endTime - startTime)) from_ to_

timeCompressReal : TimeMillis -> TimeMillis -> TimeMillis -> (Float -> a -> a -> a) -> Float -> a -> a -> a
timeCompressReal startTime endTime duration_ =
  timeCompress (toFloat startTime / toFloat duration_) (toFloat endTime / toFloat duration_)


delay : TimeMillis -> Tween a -> Tween a
delay time tween_ =
  { from = tween_.from
  , to = tween_.to
  , duration = time + tween_.duration
  , interpolation = timeCompressReal time (time+tween_.duration) (time+tween_.duration) tween_.interpolation
  }


groupTween : List (Tween a) -> Tween (List a)
groupTween tweens =
  let
    allJust : List (Maybe a) -> Maybe (List a)
    allJust = List.foldr (Maybe.map2 (::)) (Just [])

    maxDuration : TimeMillis
    maxDuration = List.map .duration tweens |> List.maximum |> Maybe.withDefault 0

    tweenInterpolation : Tween a -> Float -> a -> a -> a
    tweenInterpolation t = timeCompressReal 0 t.duration maxDuration t.interpolation

    tweenInterpolations = List.map tweenInterpolation tweens

    applyInterpolations : List (Float -> a -> a -> a) -> Float -> List a -> List a -> List a
    applyInterpolations interpolations internal from_ to_ =
      case (interpolations, from_, to_) of
        (i :: is, f :: fs, t :: ts) -> (i internal f t) :: (applyInterpolations is internal fs ts)
        _ -> []

    interpolation_ : Float -> List a -> List a -> List a
    interpolation_ = applyInterpolations tweenInterpolations
  in
  { from = allJust <| List.map .from tweens
  , to = List.map .to tweens
  , duration = maxDuration
  , interpolation = interpolation_
  }
