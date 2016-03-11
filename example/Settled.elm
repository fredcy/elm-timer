module Main (..) where

import Effects exposing (Effects)
import Html
import Html.Events
import Signal.Time
import StartApp
import Task
import Time


type alias Model =
  { input : String
  , debounced : String
  }


init : Model
init =
  Model "" ""


type Action
  = Update String
  | Settled
  | NoOp


timeToSettle =
  1000 * Time.millisecond


inputMailbox =
  Signal.mailbox ()


inputSettled =
  Signal.Time.settledAfter timeToSettle inputMailbox.signal
    |> Signal.map (always Settled)


{-| Have model.debounced track the model.input value with "de-bouncing".  User
input causes the Update action. On each Update we send to the inputMailbox,
which is then moderated by `settleAfter` and results in a Settled action.
-}
update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action |> Debug.log "action" of
    Update val ->
      ( { model | input = val }
      , Signal.send inputMailbox.address () |> Task.map (always NoOp) |> Effects.task
      )

    Settled ->
      ( { model | debounced = model.input }, Effects.none )

    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.h3 [] [ Html.text "Input here" ]
    , Html.input [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << Update) ] []
    , Html.h3 [] [ Html.text "Debounced value" ]
    , Html.div [] [ Html.text model.debounced ]
    ]


main : Signal Html.Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = ( init, Effects.none )
    , update = update
    , view = view
    , inputs = [ inputSettled ]
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
