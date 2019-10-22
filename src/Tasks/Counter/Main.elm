module Tasks.Counter.Main exposing (main)

import Browser
import Html exposing (button, div, input, text)
import Html.Attributes exposing (readonly, type_, value)
import Html.Events exposing (onClick)


main =
    Browser.sandbox
        { init = 0
        , update = \_ count -> count + 1
        , view = view
        }


view count =
    div []
        [ input [ type_ "text", value (String.fromInt count), readonly True ] []
        , button [ onClick () ] [ text "Count" ]
        ]
