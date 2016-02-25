module Timer (Model, Action, Context, init, update, start) where

{-| Provide fine-grained timer component for use in Elm Architecture apps.

# Functions
@docs init, update, start

# Types
@docs Action, Model

# Type aliases
@docs Context
-}

import Effects exposing (Effects)
import Task
import Time exposing (Time)


{-| The parent app must include the Timer Model in its own model.
-}
type Model
  = Idle
  | Starting Time
  | Active { previous : Time, elapsed : Time, duration : Time }


{-| Provide a Timer model in the initial idle state.
-}
init : Model
init =
  Idle


{-| The parent's update function must forward these actions to the Timer. The
type is opaque as the parent just forwards all timer actions the same way.
-}
type Action
  = Start Time
  | Tick Time
  | Expire


{-| Context is the address of a `Maibox` provided by the parent. Timer will send
to it to indicate expiration.
-}
type alias Context =
  Signal.Address ()


{-| Update the Timer: start it, advance it by one tick, expire it.
The Timer reports expiration by sending to the Context.
-}
update : Context -> Action -> Model -> ( Model, Effects Action )
update context action model =
  case action of
    Start duration ->
      case model of
        Idle ->
          ( Starting duration
          , Effects.tick Tick
          )

        Starting duration ->
          ( model, Effects.none ) |> Debug.log "warning: Timer got Start while Starting"

        Active { previous, elapsed, duration } ->
          -- Restarting an active timer. There is a Tick request outstanding.
          ( Active { previous = previous, elapsed = 0, duration = duration }
          , Effects.none
          )

    Tick time ->
      case model of
        Idle ->
          ( Idle, Effects.none ) |> Debug.log "error: Timer got Tick while Idle"

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
              -- Time is up. Notify the parent.
              ( model
              , Signal.send context () |> Task.map (always Expire) |> Effects.task
              )
            else
              ( Active { duration = duration, elapsed = newElapsed, previous = time }
              , Effects.tick Tick
              )

    Expire ->
      ( Idle, Effects.none )


{-| Start the timer, setting it to expire in the given number of milliseconds.
-}
start : Time -> Effects Action
start duration =
  Start duration |> Task.succeed |> Effects.task
