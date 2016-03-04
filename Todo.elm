module Todo where

import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import String
import Utils exposing (onInput, onEnter)


type alias Task =
  { desc : String
  , completed : Bool
  , id : Int
  }


type alias Model =
  { tasks : List Task
  , field : String
  , uid : Int
  }


newTask : String -> Int -> Task
newTask desc id =
  { desc = desc
  , completed = False
  , id = id
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
      { model
      | uid = model.uid + 1
      , field = ""
      , tasks = model.tasks ++ [ newTask desc model.uid ]
      }


toggleTask : Model -> Int -> Model
toggleTask model id =
  let
    updateTask t =
      if t.id == id then { t | completed = not t.completed } else t
  in
    { model | tasks = List.map updateTask model.tasks }


removeTask : Model -> Int -> Model
removeTask model id =
  { model | tasks = List.filter (\t -> t.id /= id) model.tasks }


view : Signal.Address Model -> Model -> Html
view address model =
  div []
    [ taskEntry address model
    , taskList address model

    -- useful for debugging purposes
    , fromElement (show model)
    ]


taskEntry : Signal.Address Model -> Model -> Html
taskEntry address model =
  input
    [ onInput address <| updateField model
    , onEnter address <| addTask model
    , value model.field
    , autofocus True
    ]
    []


taskList : Signal.Address Model -> Model -> Html
taskList address model =
  let
    item task = li [] [ taskView address model task ]
    items = List.map item model.tasks
  in
    ul [] items


taskView : Signal.Address Model -> Model -> Task -> Html
taskView address model task =
  div
    [ classList
        [ ("task", True)
        , ("is-completed", task.completed)
        ]
    ]
    [ input
        [ type' "checkbox"
        , checked task.completed
        , onClick address <| toggleTask model task.id
        ]
        []
    , span
        [ class "task__description" ]
        [ text task.desc ]
    , button
        [ class "task__remove-button"
        , onClick address <| removeTask model task.id
        ]
        [ text "x" ]
    ]


main : Signal Html
main =
  let
    inbox = Signal.mailbox { tasks = [], field = "", uid = 0 }
  in
    Signal.map (view inbox.address) inbox.signal
