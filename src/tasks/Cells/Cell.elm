module Tasks.Cells.Cell exposing
    ( Cell
    , Position
    , charToColumn
    , columnToChar
    , empty
    , fromString
    , heading
    , isAtSamePositionThan
    , position
    , positionFrom
    , toDisplayString
    , toHtmlId
    , toString
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Parser
import Regex exposing (Regex)
import Tasks.Cells.CellParser as CellParser
    exposing
        ( Contents(..)
        , Expression(..)
        )


type alias Position =
    { row : Int, column : Int }


type alias Cell =
    { position : Position
    , contents : CellContents
    }


type CellContents
    = Content Contents
    | Error String (List Parser.DeadEnd)
    | Empty
    | Heading String


position : Cell -> Position
position cell =
    cell.position


positionFrom : { x : Int, y : Int } -> Position
positionFrom { x, y } =
    Position y x


isAtSamePositionThan : Cell -> Cell -> Bool
isAtSamePositionThan cell1 cell2 =
    cell1.position == cell2.position


heading : Position -> String -> Cell
heading pos s =
    { position = pos, contents = Heading s }


empty : Position -> Cell
empty pos =
    { position = pos, contents = Empty }


toDisplayString : Cell -> String
toDisplayString { contents } =
    case contents of
        Content content ->
            contentToString content

        Error input errors ->
            "#ERROR# " ++ input

        Empty ->
            ""

        Heading s ->
            s


toString : Cell -> String
toString { contents } =
    case contents of
        Content content ->
            (case content of
                Expr expr ->
                    "="

                _ ->
                    ""
            )
                ++ contentToString content

        Error input errors ->
            input

        Empty ->
            ""

        Heading s ->
            s


contentToString : Contents -> String
contentToString content =
    case content of
        Expr expr ->
            case expr of
                EFloat f ->
                    String.fromFloat f

                EApplication { name, args } ->
                    name ++ "(" ++ String.join ", " (List.map (Expr >> contentToString) args) ++ ")"

                ECoord { row, column } ->
                    String.cons column <| String.fromInt row

                ERange { from, to } ->
                    (String.cons from.column <| String.fromInt from.row)
                        ++ ":"
                        ++ (String.cons to.column <| String.fromInt to.row)

        Text s ->
            s


columnToChar col =
    Char.fromCode (Char.toCode 'A' + col - 1)


charToColumn char =
    Char.toCode char - Char.toCode 'A' + 1


fromString : Position -> String -> Cell
fromString pos input_ =
    { position = pos
    , contents = parse <| String.trim input_
    }


parse : String -> CellContents
parse input =
    if String.isEmpty input then
        Empty

    else
        case CellParser.parseContents input of
            Ok contents ->
                Content contents

            Err errs ->
                Error input errs


toHtmlId : Cell -> String
toHtmlId cell =
    "cell-" ++ String.fromInt cell.position.row ++ "|" ++ String.fromInt cell.position.column


type alias ViewOptions msg =
    { editing : Maybe ( Cell, String )
    , onInput : String -> msg
    , onDblClick : msg
    , onBlur : msg
    , onEnd : msg
    }


cellWidth =
    150


cellHeight =
    35


view : ViewOptions msg -> Cell -> Html msg
view options ({ contents } as cell) =
    let
        { row, column } =
            cell.position

        isHeading =
            case contents of
                Heading _ ->
                    True

                _ ->
                    False

        isEditing =
            options.editing
                |> Maybe.map (\( editingCell, _ ) -> cell |> isAtSamePositionThan editingCell)
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
            [ onDoubleClick options.onDblClick ]

          else
            []
         )
            ++ styles
            ++ [ style "top" (px (row * cellHeight))
               , style "left" (px (column * cellWidth))
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
                       , onInput options.onInput
                       , onKeysDown options.onEnd
                       , onBlur options.onBlur
                       , style "z-index" "10"
                       , value <| editingValue
                       ]
                )
                []

          else
            text <| toDisplayString cell
        ]


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
