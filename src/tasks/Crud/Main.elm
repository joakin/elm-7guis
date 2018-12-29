module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed


type alias Model =
    { users : List ( Int, User )
    , selectedUser : Maybe Int
    , lastUserId : Int
    , filter : String
    , nameInput : String
    , surnameInput : String
    }


type alias User =
    { name : String, surname : String }


type Msg
    = NameInput String
    | SurnameInput String
    | FilterInput String
    | Create
    | Update
    | Delete
    | SelectUser String


init : Model
init =
    { users = []
    , selectedUser = Nothing
    , lastUserId = 0
    , filter = ""
    , nameInput = ""
    , surnameInput = ""
    }
        |> addUser (User "Emil" "Hans")
        |> addUser (User "Max" "Musterman")
        |> addUser (User "Roman" "Tisch")


addUser : User -> Model -> Model
addUser user model =
    { model
        | lastUserId = model.lastUserId + 1
        , users = model.users ++ [ ( model.lastUserId, user ) ]
    }


updateUser : Int -> User -> Model -> Model
updateUser id user model =
    { model
        | users =
            List.map
                (\( id_, user_ ) ->
                    if id == id_ then
                        ( id, user )

                    else
                        ( id_, user_ )
                )
                model.users
    }


removeUser : Int -> Model -> Model
removeUser id model =
    { model
        | users =
            List.filter
                (\( id_, user_ ) ->
                    if id == id_ then
                        False

                    else
                        True
                )
                model.users
    }


update : Msg -> Model -> Model
update msg ({ users, selectedUser, nameInput, surnameInput } as model) =
    case msg of
        NameInput name ->
            { model | nameInput = name }

        SurnameInput surname ->
            { model | surnameInput = surname }

        FilterInput filter ->
            { model | filter = filter }

        Create ->
            if valid nameInput surnameInput then
                { model | nameInput = "", surnameInput = "" }
                    |> addUser (User nameInput surnameInput)

            else
                model

        Update ->
            case ( selectedUser, valid nameInput surnameInput ) of
                ( Just id, True ) ->
                    { model | nameInput = "", surnameInput = "" }
                        |> updateUser id (User nameInput surnameInput)

                _ ->
                    model

        Delete ->
            case selectedUser of
                Just id ->
                    removeUser id model

                Nothing ->
                    model

        SelectUser str ->
            case String.toInt str of
                Just id ->
                    { model | selectedUser = Just id }

                Nothing ->
                    { model | selectedUser = Nothing }


userToString : User -> String
userToString { name, surname } =
    surname ++ ", " ++ name


valid : String -> String -> Bool
valid name surname =
    (not <| String.isEmpty name) && (not <| String.isEmpty surname)



-- Views


view : Model -> Html Msg
view { users, selectedUser, filter, nameInput, surnameInput } =
    column [ style "flex" "1", style "width" "100%" ]
        [ row [ style "flex" "1" ]
            [ column [ style "flex" "1" ]
                [ row []
                    [ text "Filter prefix:"
                    , spacer
                    , textInput [ value filter, onInput FilterInput ] []
                    ]
                , spacer
                , div [ style "flex" "1" ]
                    [ Keyed.node "select"
                        [ size 2
                        , style "width" "100%"
                        , style "height" "100%"
                        , onInput SelectUser
                        ]
                        (users
                            |> List.filter (\( id, user ) -> userToString user |> String.startsWith filter)
                            |> List.map
                                (\( id, user ) ->
                                    ( String.fromInt id
                                    , option
                                        [ value <| String.fromInt id
                                        , selected (id == Maybe.withDefault -1 selectedUser)
                                        ]
                                        [ text <| userToString user ]
                                    )
                                )
                        )
                    ]
                ]
            , spacer
            , column []
                [ div [ style "opacity" "0" ]
                    [ textInput [] [] ]
                , spacer
                , row
                    []
                    [ span [ style "flex" "1" ] [ text "Name:" ]
                    , spacer
                    , textInput [ value nameInput, onInput NameInput ] []
                    ]
                , spacer
                , row []
                    [ span [ style "flex" "1" ] [ text "Surname:" ]
                    , spacer
                    , textInput [ value surnameInput, onInput SurnameInput ] []
                    ]
                ]
            ]
        , spacer
        , row []
            [ button
                [ disabled (not <| valid nameInput surnameInput)
                , onClick Create
                ]
                [ text "Create" ]
            , spacer
            , button
                [ disabled (selectedUser == Nothing || (not <| valid nameInput surnameInput))
                , onClick Update
                ]
                [ text "Update" ]
            , spacer
            , button
                [ disabled (selectedUser == Nothing)
                , onClick Delete
                ]
                [ text "Delete" ]
            ]
        ]


textInput attrs children =
    input ([ type_ "text", style "flex" "1", style "max-width" "100%" ] ++ attrs) children


row attrs children =
    div ([ style "display" "flex", style "flex-direction" "row" ] ++ attrs) children


column attrs children =
    div ([ style "display" "flex", style "flex-direction" "column" ] ++ attrs) children


box attrs children =
    div (style "padding" "0.5rem" :: attrs) children


spacer =
    div [ style "padding" "0.25rem" ] []



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
