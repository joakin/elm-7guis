module Tasks.Cells.Position exposing (Position, fromXY, toXY)


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


columnToChar col =
    Char.fromCode (Char.toCode 'A' + col - 1)


charToColumn char =
    Char.toCode char - Char.toCode 'A' + 1
