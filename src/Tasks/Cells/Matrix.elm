module Tasks.Cells.Matrix exposing
    ( Matrix
    , empty
    , get
    , initialize
    , set
    , toList
    )

import Array exposing (Array)
import Tasks.Cells.Position as Position exposing (Position)


type Matrix a
    = Matrix { rows : Int, cols : Int, contents : Array a }


empty : Matrix a
empty =
    Matrix { rows = 0, cols = 0, contents = Array.empty }


initialize : Int -> Int -> (Position -> a) -> Matrix a
initialize rows cols fn =
    let
        size =
            rows * cols

        matrix =
            { rows = rows, cols = cols, contents = Array.empty }
    in
    if rows > 0 && cols > 0 then
        Matrix
            { matrix
                | contents =
                    Array.initialize size (\i -> fn (coordsFromIdx i matrix))
            }

    else
        empty


set : Position -> a -> Matrix a -> Matrix a
set pos value ((Matrix matrix) as m) =
    let
        { x, y } =
            Position.toXY pos
    in
    if
        (x >= 0 && x < matrix.cols)
            && (y >= 0 && y < matrix.rows)
    then
        Matrix
            { matrix
                | contents =
                    Array.set (coordsToIdx pos matrix) value matrix.contents
            }

    else
        m


get : Position -> Matrix a -> Maybe a
get pos (Matrix matrix) =
    let
        { x, y } =
            Position.toXY pos
    in
    if
        (x >= 0 && x < matrix.cols)
            && (y >= 0 && y < matrix.rows)
    then
        Array.get (coordsToIdx pos matrix) matrix.contents

    else
        Nothing


toList : Matrix a -> List a
toList (Matrix matrix) =
    Array.toList matrix.contents


coordsToIdx : Position -> { a | cols : Int } -> Int
coordsToIdx pos matrix =
    let
        { x, y } =
            Position.toXY pos
    in
    y * matrix.cols + x


coordsFromIdx : Int -> { a | cols : Int, rows : Int } -> Position
coordsFromIdx i matrix =
    let
        x =
            i |> modBy matrix.cols

        y =
            i // matrix.cols
    in
    Position.fromXY { x = x, y = y }
