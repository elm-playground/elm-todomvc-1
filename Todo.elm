import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : Signal.Address String -> String -> Html
view address name =
  div []
    [ input
        [ on "input" targetValue <| Signal.message address
        , value name
        , autofocus True
        ]
        []
    , text <| "Hello, " ++ name ++ "!"
    ]


main : Signal Html
main =
  let
    inbox = Signal.mailbox ""
  in
    Signal.map (view inbox.address) inbox.signal
