module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tasks.FlightBooker.FlightKind as FlightKind exposing (FlightKind(..))
import Tasks.FlightBooker.ValidatedDate as Date exposing (ValidatedDate)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = defaultForm
        , update = update
        , view = view
        }



-- TYPES


type Model
    = Form BookingForm
    | Booked BookingForm


type alias BookingForm =
    { kind : FlightKind
    , from : ValidatedDate
    , to : ValidatedDate
    }


type Msg
    = FlightKindChange String
    | FromDateInput String
    | ToDateInput String
    | Book


defaultForm =
    Form
        { kind = OneWay
        , from = Date.fromString "28.12.2018"
        , to = Date.fromString "28.12.2018"
        }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( Form booking, FlightKindChange value ) ->
            Form { booking | kind = FlightKind.fromString value }

        ( Form booking, FromDateInput value ) ->
            Form { booking | from = Date.fromString value }

        ( Form booking, ToDateInput value ) ->
            Form { booking | to = Date.fromString value }

        ( Form booking, Book ) ->
            Booked booking

        ( Booked bookingInfo, _ ) ->
            model



-- VIEWS


view : Model -> Html Msg
view model =
    case model of
        Form booking ->
            viewForm booking

        Booked bookingInfo ->
            viewBooked bookingInfo


viewForm : BookingForm -> Html Msg
viewForm { kind, from, to } =
    let
        canBook =
            (kind == OneWay && Date.isValid from)
                || (kind
                        == Return
                        && Date.isValid from
                        && Date.isValid to
                        && (from |> Date.isEarlierThan to)
                   )
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ viewFlightKind kind
        , viewValidatedDate from False FromDateInput
        , viewValidatedDate to (kind == OneWay) ToDateInput
        , button
            [ disabled (not canBook)
            , style "opacity"
                (if not canBook then
                    "0.5"

                 else
                    "1"
                )
            , onClick Book
            ]
            [ text "Book" ]
        ]


viewFlightKind : FlightKind -> Html Msg
viewFlightKind selectedKind =
    let
        option_ kind =
            option [ value (FlightKind.toString kind), selected (selectedKind == kind) ]
                [ text (FlightKind.toString kind) ]
    in
    select [ onInput FlightKindChange ]
        [ option_ OneWay
        , option_ Return
        ]


viewValidatedDate : ValidatedDate -> Bool -> (String -> Msg) -> Html Msg
viewValidatedDate date isDisabled onInputMsg =
    input
        [ type_ "text"
        , value (Date.toString date)
        , onInput onInputMsg
        , disabled isDisabled
        , style "background-color"
            (if isDisabled then
                "white"

             else if not (Date.isValid date) then
                "rgba(255,0,0,0.4)"

             else
                "white"
            )
        , style "opacity"
            (if isDisabled then
                "0.5"

             else
                "1"
            )
        ]
        []


viewBooked : BookingForm -> Html Msg
viewBooked { kind, from, to } =
    p []
        [ text <|
            "You booked a "
                ++ FlightKind.toString kind
                ++ (case kind of
                        OneWay ->
                            " on "
                                ++ Date.toString from

                        Return ->
                            " from "
                                ++ Date.toString from
                                ++ " to "
                                ++ Date.toString to
                   )
        ]
