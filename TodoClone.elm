module TodoClone (..) where

import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Events exposing (..)
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


updateField : Model -> String -> Model
updateField model string =
  { model | field = string }


inputText : Signal.Address Model -> Model -> Html
inputText address model =
  input
    [ type' "text"
    , onInput address (updateField model)
    ]
    []


view : Signal.Address Model -> Model -> Html
view address model =
  div
    []
    [ inputText address model
    , model |> show |> fromElement
    ]


main : Signal Html
main =
  let
    inbox =
      Signal.mailbox { tasks = [], field = "", uid = 0 }
  in
    Signal.map (view inbox.address) inbox.signal
