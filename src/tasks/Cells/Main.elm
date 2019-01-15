module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Task
import Tasks.Cells.Cell as Cell exposing (Cell)
import Tasks.Cells.Cell.Parser exposing (Contents, Expression)
import Tasks.Cells.Matrix as Matrix exposing (Matrix)
import Tasks.Cells.Position as Position exposing (Position)


{-| TODO:

  - Keep track of dependencies in the matrix
  - Update dependents on cell udpate

-}
type alias Model =
    { cells : Matrix Cell
    , editing : Maybe ( Cell, String )
    }


type Msg
    = CellClicked Cell
    | CellInput Cell String
    | CellBlur Cell
    | CellInputEnd Cell
    | Noop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { editing = Nothing
      , cells =
            Matrix.initialize 100
                (Char.toCode 'Z' - Char.toCode 'A' + 1)
                (\position ->
                    let
                        { x, y } =
                            Position.toXY position
                    in
                    case ( x, y ) of
                        ( 0, 0 ) ->
                            Cell.heading position ""

                        ( _, 0 ) ->
                            Cell.heading position (String.fromChar position.column)

                        ( 0, _ ) ->
                            Cell.heading position (String.fromInt position.row)

                        _ ->
                            Cell.empty position
                )
      }
    , Cmd.none
    )


updateCell : Cell -> String -> Model -> Model
updateCell cell value model =
    let
        position =
            Cell.position cell
    in
    { model | cells = Matrix.set position (Cell.fromString position value) model.cells }


getCell : Position -> Model -> Cell
getCell position model =
    model.cells
        |> Matrix.get position
        |> Maybe.withDefault (Cell.empty position)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "msg" msg
    -- in
    case msg of
        CellClicked cell ->
            ( { model | editing = Just ( cell, Cell.toString cell ) }
            , Dom.focus (Cell.toHtmlId cell)
                |> Task.attempt (\_ -> Noop)
            )

        CellInput cell input ->
            ( case model.editing of
                Just ( editingCell, s ) ->
                    if cell |> Cell.isAtSamePositionThan editingCell then
                        { model | editing = Just ( cell, input ) }

                    else
                        model

                Nothing ->
                    model
            , Cmd.none
            )

        CellBlur cell ->
            ( case model.editing of
                Just ( editingCell, input ) ->
                    if cell |> Cell.isAtSamePositionThan editingCell then
                        { model | editing = Nothing }
                            |> updateCell cell input

                    else
                        model

                Nothing ->
                    model
            , Cmd.none
            )

        CellInputEnd cell ->
            ( model
            , Dom.blur (Cell.toHtmlId cell)
                |> Task.attempt (\_ -> Noop)
            )

        Noop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        get =
            \coord -> Matrix.get coord model.cells
    in
    Keyed.node "div"
        []
        (model.cells
            |> Matrix.toList
            |> List.map
                (\cell ->
                    viewCell model.editing get cell
                )
        )


viewCell : Maybe ( Cell, String ) -> (Position -> Maybe Cell) -> Cell -> ( String, Html Msg )
viewCell editing get cell =
    let
        key =
            Cell.toHtmlId cell
    in
    ( key
    , Cell.view
        { editing = editing
        , getCell = get
        , onInput = CellInput cell
        , onDblClick = CellClicked cell
        , onBlur = CellBlur cell
        , onEnd = CellInputEnd cell
        }
        cell
    )
