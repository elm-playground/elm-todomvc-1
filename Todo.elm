import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Utils exposing (onInput, onEnter)


type alias Model =
  { tasks : List String
  , field : String
  }


updateField : Model -> String -> Model
updateField model field =
  { model | field = field }


addTask : Model -> Model
addTask model =
  let
    desc = String.trim model.field
  in
    if String.isEmpty desc then
      model
    else
      { model | field = "" , tasks = model.tasks ++ [ desc ] }


view : Signal.Address Model -> Model -> Html
view address model =
  let
    items = List.map (\task -> li [] [ text task ]) model.tasks
  in
    div []
      [ input
          [ onInput address <| updateField model
          , onEnter address <| addTask model
          , value model.field
          , autofocus True
          ]
          []
      , text <| "Hello, " ++ model.field ++ "!"
      , ul [] items
      ]


main : Signal Html
main =
  let
    inbox = Signal.mailbox { tasks = [], field = "" }
  in
    Signal.map (view inbox.address) inbox.signal
