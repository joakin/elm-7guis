module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tasks.TemperatureConverter.Temperature as Temperature exposing (Temperature)


type alias Model =
    { temperature : Temperature
    , celsius : TemperatureInput
    , farenheit : TemperatureInput
    }


type TemperatureInput
    = Valid
    | Invalid String


type Unit
    = Celsius
    | Farenheit


type Msg
    = Update Unit String


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { temperature = Temperature.fromCelsius 0
            , celsius = Invalid ""
            , farenheit = Invalid ""
            }
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


updateTemperature : Unit -> Float -> Model -> Model
updateTemperature temperatureUnit value model =
    { temperature =
        case temperatureUnit of
            Celsius ->
                Temperature.fromCelsius value

            Farenheit ->
                Temperature.fromFarenheit value

    -- When the temperature input by the user is valid, mark both input fields
    -- as valid to be updated
    , celsius = Valid
    , farenheit = Valid
    }


updateAsString : Unit -> String -> Model -> Model
updateAsString temperatureUnit value model =
    case temperatureUnit of
        Celsius ->
            { model | celsius = Invalid value }

        Farenheit ->
            { model | farenheit = Invalid value }


view : Model -> Html Msg
view { temperature, celsius, farenheit } =
    div []
        [ temperatureInput celsius Celsius temperature
        , span [] [ text " Celsius" ]
        , span [] [ text " = " ]
        , temperatureInput farenheit Farenheit temperature
        , span [] [ text " Farenheit" ]
        ]


temperatureInput : TemperatureInput -> Unit -> Temperature -> Html Msg
temperatureInput field unit temperature =
    let
        strValue =
            case field of
                Valid ->
                    String.fromFloat <|
                        case unit of
                            Celsius ->
                                Temperature.toCelsius temperature

                            Farenheit ->
                                Temperature.toFarenheit temperature

                Invalid str ->
                    str
    in
    input [ type_ "text", value strValue, onInput (Update unit) ] []
