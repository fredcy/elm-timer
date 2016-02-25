module Main (..) where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task
import Time exposing (Time)
import Timer


type alias Model =
  { input : String
  , debounced : String
  , timer : Timer.Model
  }


type Action
  = Update String
  | TimerAction Timer.Action
  | Timeout
  | TimerTick Time


{-| Have model.debounced track the model.input value with "de-bouncing".  User
input causes the Update action. On each Update we start (or restart) the
timer. We pass an address to Timer.update which it sends to when the timer
expires.
-}
update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action |> Debug.log "action" of
    Update val ->
      ( { model | input = val }
      , Timer.start 500 |> Effects.map TimerAction
      )

    TimerAction timerAction ->
      let
        handleTimerTick t =
          if t <= 0 then
            Timeout
          else
            TimerTick t

        context =
          Signal.forwardTo actionsMailbox.address handleTimerTick

        ( newTimer, timerEffect ) =
          Timer.update context timerAction model.timer
      in
        ( { model | timer = newTimer }
        , Effects.map TimerAction timerEffect
        )

    Timeout ->
      ( { model | debounced = model.input }, Effects.none )

    TimerTick t ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << Update) ] []
    , Html.div [] [ Html.text model.debounced ]
    ]


main : Signal Html.Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = ( Model "" "" Timer.init, Effects.none )
    , update = update
    , view = view
    , inputs = [ actionsMailbox.signal ]
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


actionsMailbox : Signal.Mailbox Action
actionsMailbox =
  Signal.mailbox Timeout
