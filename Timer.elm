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
type alias Model =
  { state : State
  , remaining : Time
  }


type State
  = Idle
  | Starting
  | Active { previous : Time }


{-| Provide a Timer model in the initial idle state.
-}
init : Model
init =
  Model Idle 0


{-| The parent's update function must forward these actions to the Timer. The
type is opaque as the parent just forwards all timer actions the same way.
-}
type Action
  = Start Time
  | Tick Time
  | NoOp


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
      case model.state of
        Idle ->
          ( { model | remaining = duration, state = Starting }
          , Effects.tick Tick
          )

        Starting ->
          ( model, Effects.none ) |> Debug.log "warning: Timer got Start while Starting"

        Active { previous } ->
          -- Restarting an active timer. There is a Tick request outstanding.
          ( { model | remaining = duration }
          , Effects.none
          )

    Tick time ->
      case model.state of
        Idle ->
          ( model, Effects.none ) |> Debug.log "error: Timer got Tick while Idle"

        Starting ->
          ( { model | state = Active { previous = time } }
          , Effects.tick Tick
          )

        Active { previous } ->
          let
            newRemaining =
              model.remaining - (time - previous) |> max 0
          in
            if newRemaining == 0 then
              ( { model | remaining = 0, state = Idle }
              , Signal.send context () |> Task.map (always NoOp) |> Effects.task
              )
            else
              ( { model | remaining = newRemaining, state = Active { previous = time } }
              , Effects.tick Tick
              )

    NoOp ->
      ( model, Effects.none )


{-| Start the timer, setting it to expire in the given number of milliseconds.
-}
start : Time -> Effects Action
start duration =
  Start duration |> Task.succeed |> Effects.task
