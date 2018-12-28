module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { celsius : String, farenheit : String }


type Msg
    = Update TemperatureUnit String


type TemperatureUnit
    = Celsius
    | Farenheit


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { celsius = "", farenheit = "" }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update temperatureUnit value ->
            case String.toFloat value of
                Just num ->
                    updateTemperature temperatureUnit num model

                Nothing ->
                    updateAsString temperatureUnit value model


updateTemperature : TemperatureUnit -> Float -> Model -> Model
updateTemperature temperatureUnit value { celsius, farenheit } =
    case temperatureUnit of
        Celsius ->
            { celsius = String.fromFloat value
            , farenheit = String.fromFloat <| value * (9 / 5) + 32
            }

        Farenheit ->
            { celsius = String.fromFloat <| (value - 32) * (5 / 9)
            , farenheit = String.fromFloat value
            }


updateAsString : TemperatureUnit -> String -> Model -> Model
updateAsString temperatureUnit value model =
    case temperatureUnit of
        Celsius ->
            { model | celsius = value }

        Farenheit ->
            { model | farenheit = value }


view : Model -> Html Msg
view { celsius, farenheit } =
    div []
        [ input [ type_ "text", value celsius, onInput (Update Celsius) ] []
        , span [] [ text " Celsius" ]
        , span [] [ text " = " ]
        , input [ type_ "text", value farenheit, onInput (Update Farenheit) ] []
        , span [] [ text " Farenheit" ]
        ]
