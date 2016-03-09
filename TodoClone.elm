module TodoClone (..) where

import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils exposing (onInput, onEnter)

type alias Task =
  { desc : String,
    completed : Bool,
    id : Int }

type alias Model =
  { tasks : List Task,
    field : String,
    uid : Int }

updateField : Model -> String -> Model
updateField model string =
  { model | field = string }

newTask : String -> Int -> Task
newTask desc id =
  { desc = desc,
    completed = False,
    id = id }

addTask : Model -> Model
addTask model =
  { model |
      uid = model.uid + 10,
      field = "",
      tasks = model.tasks ++ [ newTask model.field model.uid ] }

inputText : Signal.Address Model -> Model -> Html
inputText address model =
  input
    [ type' "text",
      onInput address (updateField model),
      onEnter address (addTask model),
      value model.field ]
    []

view : Signal.Address Model -> Model -> Html
view address model =
  div
    []
    [ inputText address model,
      model |> show |> fromElement ]

main : Signal Html
main =
  let
    inbox =
      Signal.mailbox { tasks = [], field = "", uid = 0 }
  in
    Signal.map (view inbox.address) inbox.signal
