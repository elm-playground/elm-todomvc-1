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


view : Signal.Address Model -> Model -> Html
view address model =
  let
    item task =
      li []
        [ input
            [ type' "checkbox"
            , checked task.completed
            , onClick address <| toggleTask model task.id
            ]
            []
        , span
            [ classList [ ("strikethrough", task.completed) ] ]
            [ text task.desc ]
        ]

    items = List.map item model.tasks
  in
    div []
      [ input
          [ onInput address <| updateField model
          , onEnter address <| addTask model
          , value model.field
          , autofocus True
          ]
          []
      , ul [] items

      -- useful for debugging purposes
      , fromElement (show model)
      ]


main : Signal Html
main =
  let
    inbox = Signal.mailbox { tasks = [], field = "", uid = 0 }
  in
    Signal.map (view inbox.address) inbox.signal
