module Timer (Model, Action, init, update, start) where

{-| Timer provides a fine-grained timer component for use in Elm Architecture
apps. Its resolution is as good as can be obtained using Effects.tick.

See example/Main.elm in the source for an example application that uses Timer.

## Steps in adding Timer to an application

### Add Timer model to application model

```elm
type alias Model = { ..., timer : Timer.Model }
```

### Initialize the timer

```elm
init = Model ..., Timer.init
```

### Define new actions to represent timeout and timer actions

```elm
type Action = ... | TimerAction Timer.Action | Timeout
```

These action names are arbitrary (but must be consistent with their usage
below). The former represents actions that must be forwarded to the timer
component. The latter occurs when the timer expires.

### Define a Mailbox for receiving the expiration event

```elm
actionsMailbox =
  Signal.mailbox Timeout
```

### Add the Mailbox signal to app inputs

```elm
app = StartApp.start { ..., inputs = [ actionMailbox.signal ] }
```

### Start timer as an effect in the update function

```elm
update action model =
  case action of
    ...
    SomeTriggerAction -> ( model
                         , Timer.start 500 |> Effects.map TimerAction
                         )
```

This could also be done as an effect in the app's init function.

### Handle timeout as new case in the update function

```elm
    Timeout -> ( someChangeTo model, Effects.none )
```
This could return some other effect if that's what is desired when timeout occurs.


### Forward actions and effects to and from timer in update function

```elm
    TimerAction timerAction ->
      let
        context =
          Signal.forwardTo actionsMailbox.address (always Timeout)

        ( newTimer, timerEffect ) =
          Timer.update context timerAction model.timer
      in
        ( { model | timer = newTimer }
        , Effects.map TimerAction timerEffect
        )
```

# Functions
@docs init, update, start

# Types
@docs Action, Model

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


{-| Start the timer, setting it to expire in the given number of
milliseconds. The resulting effect must be the `Action` value that forwards to
the timer, `TimerAction` in the examples.
-}
start : Time -> Effects Action
start duration =
  Start duration |> Task.succeed |> Effects.task
