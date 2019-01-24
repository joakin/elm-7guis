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
import Tasks.Cells.Dependencies as Dependencies exposing (Dependencies)
import Tasks.Cells.Matrix as Matrix exposing (Matrix)
import Tasks.Cells.Position as Position exposing (Position)


type alias Model =
    { cells : Matrix Cell
    , dependencies : Dependencies
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
      , dependencies = Dependencies.empty
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
        |> setInitialCells
    , Cmd.none
    )


setInitialCells : Model -> Model
setInitialCells model =
    let
        initialCells : List ( Char, Int, String )
        initialCells =
            [ ( 'A', 1, "Who" )
            , ( 'B', 1, "How much" )
            , ( 'A', 2, "Jane" )
            , ( 'B', 2, "=10" )
            , ( 'A', 3, "John" )
            , ( 'B', 3, "=8" )
            , ( 'A', 4, "Maria" )
            , ( 'B', 4, "=5.5" )
            , ( 'A', 5, "Adam" )
            , ( 'B', 5, "=13.4" )
            , ( 'D', 1, "Total" )
            , ( 'E', 1, "=sum(B2:B5)" )
            , ( 'D', 2, "Average" )
            , ( 'E', 2, "=div(sum(B2:B5), 5)" )
            ]
    in
    List.foldl
        (\( col, row, str ) m ->
            updateCellFromString (Cell.empty (Position col row)) str m
        )
        model
        initialCells



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClicked cell ->
            ( { model | editing = Just ( cell, Cell.toEditableString cell ) }
            , Dom.focus (Cell.toHtmlId cell)
                |> Task.attempt (\_ -> Noop)
            )

        CellInput cell input ->
            ( case model.editing of
                Just ( editingCell, s ) ->
                    if cell.position == editingCell.position then
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
                    if cell.position == editingCell.position then
                        { model | editing = Nothing }
                            |> updateCellFromString cell input

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


updateCellFromString : Cell -> String -> Model -> Model
updateCellFromString cell value model =
    let
        get =
            \coord -> Matrix.get coord model.cells

        newCell =
            Cell.fromString get cell.position value

        newDependencies =
            Dependencies.updateDependenciesFor cell.position
                { old = Cell.dependencies cell
                , new = Cell.dependencies newCell
                }
                model.dependencies
    in
    { model
        | cells = Matrix.set cell.position newCell model.cells
        , dependencies =
            Dependencies.updateDependenciesFor cell.position
                { old = Cell.dependencies cell
                , new = Cell.dependencies newCell
                }
                model.dependencies
    }
        |> propagateChanges cell.position


propagateChanges : Position -> Model -> Model
propagateChanges changedPosition model =
    let
        dependents =
            Dependencies.getDependentsOn changedPosition model.dependencies
                |> List.filterMap (\pos -> Matrix.get pos model.cells)
    in
    List.foldl refreshCell model dependents


refreshCell : Cell -> Model -> Model
refreshCell cell model =
    let
        get =
            \coord -> Matrix.get coord model.cells

        newCell =
            Cell.refresh get cell
    in
    { model | cells = Matrix.set cell.position newCell model.cells }
        |> propagateChanges cell.position



-- VIEW


view : Model -> Html Msg
view model =
    Keyed.node "div"
        []
        (model.cells
            |> Matrix.toList
            |> List.map (viewCell model.editing)
        )


viewCell : Maybe ( Cell, String ) -> Cell -> ( String, Html Msg )
viewCell editing cell =
    let
        key =
            Cell.toHtmlId cell
    in
    ( key
    , Cell.view
        { editing = editing
        , onInput = CellInput
        , onDblClick = CellClicked
        , onBlur = CellBlur
        , onEnd = CellInputEnd
        }
        cell
    )
