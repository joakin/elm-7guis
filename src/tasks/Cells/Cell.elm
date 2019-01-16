module Tasks.Cells.Cell exposing
    ( Cell
    , dependencies
    , empty
    , fromString
    , heading
    , toEditableString
    , toHtmlId
    , view
    )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Parser
import Regex exposing (Regex)
import Tasks.Cells.Cell.Parser as Parser
    exposing
        ( Contents(..)
        , Expression(..)
        , Range
        )
import Tasks.Cells.Position as Position exposing (Position)


type alias Cell =
    { position : Position
    , value : Type
    }


type Type
    = Formula { expression : Expression, dependencies : List Position, result : Maybe Float }
    | Error String (List Parser.DeadEnd)
    | Text String
    | Empty
    | Heading String



-- CREATING CELLS


heading : Position -> String -> Cell
heading pos s =
    { position = pos, value = Heading s }


empty : Position -> Cell
empty pos =
    { position = pos, value = Empty }


fromString : (Position -> Maybe Cell) -> Position -> String -> Cell
fromString getCell pos input_ =
    { position = pos
    , value = parse getCell <| String.trim input_
    }


parse : (Position -> Maybe Cell) -> String -> Type
parse getCell input =
    if String.isEmpty input then
        Empty

    else
        case Parser.parseContents input of
            Ok (Parser.Text str) ->
                Text str

            Ok (Parser.Expr expr) ->
                Formula
                    { expression = expr
                    , result = evaluate getCell expr
                    , dependencies = getDependenciesFromExpression expr
                    }

            Err errs ->
                Error input errs



-- CONVERTING CELLS TO STRING


toDisplayString : Cell -> String
toDisplayString { value } =
    case value of
        Formula formula ->
            formula.result
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault "#NAN#"

        Text txt ->
            txt

        Error input errors ->
            "#ERROR# " ++ input

        Empty ->
            ""

        Heading s ->
            s


toEditableString : Cell -> String
toEditableString { value } =
    case value of
        Formula formula ->
            "=" ++ formulaToString formula.expression

        Text txt ->
            txt

        Error input errors ->
            input

        Empty ->
            ""

        Heading s ->
            s


formulaToString : Expression -> String
formulaToString expr =
    case expr of
        EFloat f ->
            String.fromFloat f

        EApplication { name, args } ->
            name ++ "(" ++ String.join ", " (List.map formulaToString args) ++ ")"

        ECoord { row, column } ->
            String.cons column <| String.fromInt row

        ERange { from, to } ->
            (String.cons from.column <| String.fromInt from.row)
                ++ ":"
                ++ (String.cons to.column <| String.fromInt to.row)



-- VIEW


type alias ViewOptions msg =
    { editing : Maybe ( Cell, String )
    , onInput : Cell -> String -> msg
    , onDblClick : Cell -> msg
    , onBlur : Cell -> msg
    , onEnd : Cell -> msg
    }


cellWidth =
    150


cellHeight =
    35


view : ViewOptions msg -> Cell -> Html msg
view options cell =
    let
        { x, y } =
            Position.toXY cell.position

        isHeading =
            case cell.value of
                Heading _ ->
                    True

                _ ->
                    False

        isEditing =
            options.editing
                |> Maybe.map (\( editingCell, _ ) -> cell.position == editingCell.position)
                |> Maybe.withDefault False

        editingValue =
            options.editing
                |> Maybe.map (\( _, editingContents ) -> editingContents)
                |> Maybe.withDefault ""

        styles =
            [ style "padding" "0 0.5rem"
            , style "width" (px cellWidth)
            , style "height" (px cellHeight)
            , style "line-height" (px cellHeight)
            , style "vertical-align" "middle"
            , style "position" "absolute"
            , style "box-sizing" "border-box"
            ]
    in
    div
        ((if not isEditing && not isHeading then
            [ onDoubleClick <| options.onDblClick cell ]

          else
            []
         )
            ++ styles
            ++ [ style "top" (px (y * cellHeight))
               , style "left" (px (x * cellWidth))
               , style "border"
                    (if isHeading then
                        "1px solid gray"

                     else
                        "1px solid lightgray"
                    )
               , style "background-color"
                    (if isHeading then
                        "lightgray"

                     else
                        "white"
                    )
               ]
        )
        [ if isEditing && not isHeading then
            input
                (styles
                    ++ [ style "top" (px 0)
                       , style "left" (px 0)
                       , id <| toHtmlId cell
                       , onInput <| options.onInput cell
                       , onKeysDown <| options.onEnd cell
                       , onBlur <| options.onBlur cell
                       , style "z-index" "10"
                       , value <| editingValue
                       ]
                )
                []

          else
            text <| toDisplayString cell
        ]


toHtmlId : Cell -> String
toHtmlId cell =
    "cell-" ++ Position.toString cell.position


px n =
    String.fromInt n ++ "px"


onKeysDown : msg -> Attribute msg
onKeysDown msg =
    let
        filterKey pressedCode =
            if
                (pressedCode == escapeKeyCode)
                    || (pressedCode == enterKeyCode)
            then
                Decode.succeed ( msg, True )

            else
                Decode.fail "Ignored input"

        decoder =
            keyCode
                |> Decode.andThen filterKey
    in
    preventDefaultOn "keydown" decoder


enterKeyCode : Int
enterKeyCode =
    13


escapeKeyCode : Int
escapeKeyCode =
    27



-- DEPENDENCIES


dependencies : Cell -> List Position
dependencies cell =
    case cell.value of
        Formula formula ->
            formula.dependencies

        _ ->
            []


getDependenciesFromExpression : Expression -> List Position
getDependenciesFromExpression expr =
    case expr of
        EFloat f ->
            []

        ECoord pos ->
            [ pos ]

        ERange range ->
            expandRange range

        EApplication { name, args } ->
            args
                |> List.concatMap getDependenciesFromExpression



-- EVALUATION


evaluate : (Position -> Maybe Cell) -> Expression -> Maybe Float
evaluate get expr =
    case expr of
        EFloat f ->
            Just f

        ECoord pos ->
            case get pos of
                Just cell ->
                    evaluateCell get cell

                Nothing ->
                    Nothing

        ERange { from, to } ->
            -- Ranges expand to nothing as a top level, only work as arguments
            Nothing

        EApplication { name, args } ->
            let
                maybeArguments : Maybe (List Float)
                maybeArguments =
                    args
                        |> List.concatMap (evaluateArgument get)
                        |> combineMaybes
            in
            case ( Dict.get name functions, maybeArguments ) of
                ( Just fn, Just arguments ) ->
                    fn arguments

                _ ->
                    Nothing


evaluateArgument : (Position -> Maybe Cell) -> Expression -> List (Maybe Float)
evaluateArgument get arg =
    case arg of
        ERange range ->
            expandRange range
                |> List.map
                    (\coord ->
                        get coord
                            |> Maybe.andThen (evaluateCell get)
                    )

        _ ->
            [ evaluate get arg ]


evaluateCell : (Position -> Maybe Cell) -> Cell -> Maybe Float
evaluateCell get { value } =
    case value of
        Formula formula ->
            evaluate get formula.expression

        _ ->
            Nothing


functions : Dict String (List Float -> Maybe Float)
functions =
    Dict.empty
        |> Dict.insert "add"
            (\list ->
                case list of
                    x :: y :: [] ->
                        Just (x + y)

                    _ ->
                        Nothing
            )
        |> Dict.insert "sub"
            (\list ->
                case list of
                    x :: y :: [] ->
                        Just (x - y)

                    _ ->
                        Nothing
            )
        |> Dict.insert "div"
            (\list ->
                case list of
                    x :: y :: [] ->
                        Just (x / y)

                    _ ->
                        Nothing
            )
        |> Dict.insert "mul"
            (\list ->
                case list of
                    x :: y :: [] ->
                        Just (x * y)

                    _ ->
                        Nothing
            )
        |> Dict.insert "mod"
            (\list ->
                case list of
                    x :: y :: [] ->
                        Just <| toFloat (round x |> modBy (round y))

                    _ ->
                        Nothing
            )
        |> Dict.insert "sum"
            (\list -> Just <| List.foldl (+) 0 list)
        |> Dict.insert "prod"
            (\list -> Just <| List.foldl (*) 1 list)


expandRange : Range -> List Position
expandRange { from, to } =
    let
        ( from_, to_ ) =
            ( Position.toXY from, Position.toXY to )

        columns =
            List.range from_.x to_.x

        rows =
            List.range from.row to.row
    in
    columns
        |> List.concatMap
            (\x ->
                rows |> List.map (\y -> Position.fromXY { x = x, y = y })
            )


combineMaybes : List (Maybe a) -> Maybe (List a)
combineMaybes maybes =
    let
        step e acc =
            case e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldr step (Just []) maybes
