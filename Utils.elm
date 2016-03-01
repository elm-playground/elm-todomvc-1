module Utils
  ( onInput
  , onEnter, onKeyCode
  ) where

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue, keyCode)
import Json.Decode as Json


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (Signal.message address << f)


onEnter : Signal.Address a -> a -> Attribute
onEnter =
  onKeyCode 13


onKeyCode : Int -> Signal.Address a -> a -> Attribute
onKeyCode code address value =
  let
    isCode pressed =
      if pressed == code then Ok () else Err "incorrect key code"
  in
    on "keydown"
      (Json.customDecoder keyCode isCode)
      (\_ -> Signal.message address value)
