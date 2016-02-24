module Main (..) where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task
import Timer


type alias Model =
  { input : String
  , displayed : String
  , timer : Timer.Model
  }


type Action
  = Update String
  | TimerAction Timer.Action
  | UpdateDisplay String



{- Have model.displayed track the model.input value with "de-bouncing".  User
input causes the Update action. On each Update we start (or restart) the
timer. In the case where we forward actions to the timer we check for
expiration; when that occurs we create an UpdateDisplay action.  -}


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Update val ->
      ( { model | input = val }
      , Timer.start 500 |> Effects.map TimerAction
      )

    TimerAction timerAction ->
      let
        ( newTimer, timerEffect ) =
          Timer.update timerAction model.timer

        effect =
          if timerAction == Timer.Expire then
            UpdateDisplay model.input |> Task.succeed |> Effects.task
          else
            Effects.map TimerAction timerEffect
      in
        ( { model | timer = newTimer }
        , effect
        )

    UpdateDisplay val ->
      ( { model | displayed = val }, Effects.none )


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    []
    [ Html.input [ Html.Events.on "input" Html.Events.targetValue (Signal.message address << Update) ] []
    , Html.div [] [ Html.text model.displayed ]
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
    , inputs = []
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
