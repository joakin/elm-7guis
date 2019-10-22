module Tasks.Cells.Position exposing (Position, fromString, fromXY, parser, toString, toXY)

import Parser exposing ((|.), (|=), Parser, chompIf, getChompedString, int, succeed)


type alias Position =
    { column : Char
    , row : Int
    }


fromXY : { x : Int, y : Int } -> Position
fromXY { x, y } =
    Position (columnToChar x) y


toXY : Position -> { x : Int, y : Int }
toXY { column, row } =
    { x = charToColumn column, y = row }


toString : Position -> String
toString { column, row } =
    String.cons column <| String.fromInt row


fromString : String -> Maybe Position
fromString input =
    Parser.run parser input
        |> Result.toMaybe


parser : Parser Position
parser =
    succeed
        (\column row ->
            Position (String.uncons column |> Maybe.map Tuple.first |> Maybe.withDefault 'A') row
        )
        |= (getChompedString <| succeed () |. chompIf Char.isUpper)
        |= int


columnToChar col =
    Char.fromCode (Char.toCode 'A' + col - 1)


charToColumn char =
    Char.toCode char - Char.toCode 'A' + 1
