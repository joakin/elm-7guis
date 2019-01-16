module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


type Model
    = Model


type Msg
    = Msg


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    Model


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div []
        []
