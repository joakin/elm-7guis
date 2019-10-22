module Tasks.Timer.Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Round as R
import Ui exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { elapsed : Float, total : Float }


type Msg
    = Tick Float
    | SizeInput Float
    | Reset


init () =
    ( { elapsed = 0, total = 15 * 1000 }, Cmd.none )


subscriptions { elapsed, total } =
    if elapsed < total then
        onAnimationFrameDelta Tick

    else
        Sub.none


update msg model =
    case msg of
        Tick delta ->
            ( { model | elapsed = model.elapsed + delta }, Cmd.none )

        SizeInput total ->
            ( { model | total = total }, Cmd.none )

        Reset ->
            init ()


view : Model -> Html Msg
view ({ elapsed, total } as model) =
    let
        progress =
            elapsed * 100 / total
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
        , box [] [ text <| R.round 1 (elapsed / 1000) ++ "s" ]
        , box
            [ style "display" "flex"
            , style "align-items" "center"
            ]
            [ span [ style "margin-right" "0.5rem" ] [ text "Duration:" ]
            , duration [ style "flex" "1" ] total
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


duration attrs total =
    input
        ([ type_ "range"
         , Attributes.min "0"
         , Attributes.max "30000"
         , value <|
            if isInfinite total then
                "30000"

            else
                String.fromFloat total
         , step "100"
         , onInput <| SizeInput << (\s -> String.toFloat s |> Maybe.withDefault 0)
         ]
            ++ attrs
        )
        []
