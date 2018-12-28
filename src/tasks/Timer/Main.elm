module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Round exposing (round)
import Time exposing (Posix)


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { start : Int, lastTick : Int, size : Float }


type Msg
    = Tick Posix
    | SizeInput Float
    | Reset


init start =
    ( { start = start, lastTick = start, size = 15 * 1000 }, Cmd.none )


subscriptions model =
    let
        { progress } =
            getInfo model
    in
    if progress < 100 then
        -- Time.every 100 Tick
        onAnimationFrame Tick

    else
        Sub.none


update msg model =
    case msg of
        Tick posix ->
            let
                currentTime =
                    Time.posixToMillis posix
            in
            ( { model | lastTick = currentTime }, Cmd.none )

        SizeInput size ->
            ( { model | size = size }, Cmd.none )

        Reset ->
            init model.lastTick


getInfo { start, lastTick, size } =
    let
        elapsed =
            toFloat (lastTick - start)
    in
    { elapsed = elapsed / 1000, progress = elapsed * 100 / size }


view : Model -> Html Msg
view ({ size } as model) =
    let
        { elapsed, progress } =
            getInfo model
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ box
            [ style "display" "flex"
            , style "align-items" "center"
            ]
            [ span [ style "margin-right" "0.5rem" ] [ text "Elapsed time:" ]
            , progressBar [ style "flex" "1" ] progress
            ]
        , box [] [ text <| round 1 elapsed ++ "s" ]
        , box
            [ style "display" "flex"
            , style "align-items" "center"
            ]
            [ span [ style "margin-right" "0.5rem" ] [ text "Duration:" ]
            , duration [ style "flex" "1" ] size
            ]
        , box [] [ button [ style "width" "100%", onClick Reset ] [ text "Reset" ] ]
        ]


progressBar attrs val =
    progress
        ([ Attributes.max "100"
         , value <|
            if isInfinite val then
                "100"

            else
                String.fromFloat val
         ]
            ++ attrs
        )
        []


duration attrs size =
    input
        ([ type_ "range"
         , Attributes.min "0"
         , Attributes.max "30000"
         , value <|
            if isInfinite size then
                "30000"

            else
                String.fromFloat size
         , step "100"
         , onInput <| SizeInput << (\s -> String.toFloat s |> Maybe.withDefault 0)
         ]
            ++ attrs
        )
        []


box attrs children =
    div (style "padding" "0.5rem" :: attrs) children
