module Main (..) where

import Effects exposing (Effects)
import Html
import Html.Events
import StartApp
import Task


type alias Model =
  { input : String
  , debounced : String
  , sleepCount : Int
  }


init : Model
init =
  Model "" "" 0


type Action
  = Update String
  | Timeout Int


{-| Have model.debounced track the model.input value with "de-bouncing".  User
input causes the Update action. On each Update we start (or restart) the
timer.
-}
update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action |> Debug.log "action" of
    Update val ->
      let
        newCount =
          model.sleepCount + 1
      in
        ( { model | input = val, sleepCount = newCount }
        , Task.sleep 250 |> Effects.task |> Effects.map (always (Timeout newCount))
        )

    Timeout count ->
      -- Only act on the most recent sleep call.
      if count == model.sleepCount then
        ( { model | debounced = model.input, sleepCount = 0 }, Effects.none )
      else
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
    , inputs = []
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
