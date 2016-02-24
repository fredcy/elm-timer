module Timer (Model, Action(Expire), init, update, start) where

import Effects exposing (Effects)
import Task
import Time exposing (Time)


type Model
  = Idle
  | Starting Time
  | Active { previous : Time, elapsed : Time, duration : Time }


init : Model
init =
  Idle


type Action
  = Start Time
  | Tick Time
  | Expire


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Start duration ->
      case model of
        Idle ->
          ( Starting duration
          , Effects.tick Tick
          )

        Starting duration ->
          ( model, Effects.none ) |> Debug.log "got Start while starting"

        Active { previous, elapsed, duration } ->
          ( Active { previous = previous, elapsed = 0, duration = duration }
          , Effects.none
          )

    Tick time ->
      case model of
        Idle ->
          ( Idle, Effects.none ) |> Debug.log "got tick while idle"

        Starting duration ->
          ( Active { duration = duration, elapsed = 0, previous = time }
          , Effects.tick Tick
          )

        Active { previous, elapsed, duration } ->
          let
            newElapsed =
              elapsed + (time - previous)
          in
            if newElapsed >= duration then
              ( Idle
              , Effects.task (Task.succeed Expire)
              )
            else
              ( Active { duration = duration, elapsed = newElapsed, previous = time }
              , Effects.tick Tick
              )

    Expire ->
      ( Idle, Effects.none )


start : Time -> Effects Action
start duration =
  Start duration |> Task.succeed |> Effects.task
