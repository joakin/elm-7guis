module Main exposing (main)

import Html exposing (div, p, text)


main =
    div []
        (List.range 0 100
            |> List.map (always (p [] [ text "test" ]))
        )
