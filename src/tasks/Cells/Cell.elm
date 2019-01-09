module Tasks.Cells.Cell exposing
    ( Cell
    , Position
    , empty
    , fromString
    , heading
    , isAtSamePositionThan
    , position
    , positionFrom
    , toHtmlId
    , toString
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


type alias Position =
    { row : Int, column : Int }


type alias Cell =
    { position : Position
    , contents : Contents
    }


type Contents
    = Formula String
    | Text String
    | Float Float
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


toString : Cell -> String
toString { contents } =
    case contents of
        Float f ->
            String.fromFloat f

        Formula s ->
            s

        Text s ->
            s

        Empty ->
            ""

        Heading s ->
            s


fromString : Position -> String -> Cell
fromString pos input =
    { position = pos
    , contents =
        if String.isEmpty input then
            Empty

        else if String.startsWith "=" input then
            Formula input

        else
            case String.toFloat input of
                Just float ->
                    Float float

                Nothing ->
                    Text input
    }


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
            text <| toString cell
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
