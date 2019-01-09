module Tasks.Cells.Matrix exposing
    ( Matrix
    , empty
    , get
    , initialize
    , set
    , toList
    )

import Array exposing (Array)


type Matrix a
    = Matrix { rows : Int, cols : Int, contents : Array a }


empty : Matrix a
empty =
    Matrix { rows = 0, cols = 0, contents = Array.empty }


initialize : Int -> Int -> ({ x : Int, y : Int } -> a) -> Matrix a
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


set : { x : Int, y : Int } -> a -> Matrix a -> Matrix a
set ({ x, y } as coords) value ((Matrix matrix) as m) =
    if
        (x >= 0 && x < matrix.cols)
            && (y >= 0 && y < matrix.rows)
    then
        Matrix
            { matrix
                | contents =
                    Array.set (coordsToIdx coords matrix) value matrix.contents
            }

    else
        m


get : { x : Int, y : Int } -> Matrix a -> Maybe a
get ({ x, y } as coords) (Matrix matrix) =
    if
        (x >= 0 && x < matrix.cols)
            && (y >= 0 && y < matrix.rows)
    then
        Array.get (coordsToIdx coords matrix) matrix.contents

    else
        Nothing


toList : Matrix a -> List a
toList (Matrix matrix) =
    Array.toList matrix.contents


coordsToIdx : { x : Int, y : Int } -> { a | cols : Int } -> Int
coordsToIdx { x, y } matrix =
    y * matrix.cols + x


coordsFromIdx : Int -> { a | cols : Int, rows : Int } -> { x : Int, y : Int }
coordsFromIdx i matrix =
    let
        x =
            i |> modBy matrix.cols

        y =
            i // matrix.cols
    in
    { x = x, y = y }
