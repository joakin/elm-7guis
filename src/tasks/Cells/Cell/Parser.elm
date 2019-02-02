module Tasks.Cells.Cell.Parser exposing
    ( Application
    , Contents(..)
    , Expression(..)
    , Range
    , parseContents
    )

import Parser exposing (..)
import Set
import Tasks.Cells.Position as Position exposing (Position)


type alias Range =
    { from : Position, to : Position }


positionOrRange : Position -> Parser Expression
positionOrRange pos =
    oneOf
        [ succeed (ERange << Range pos)
            |. symbol ":"
            |= Position.parser
        , succeed (ECoord pos)
        ]


float : Parser Float
float =
    number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


type alias Application =
    { name : String, args : List Expression }


application : Parser Application
application =
    succeed Application
        |= variable { start = Char.isAlpha, inner = \c -> Char.isAlpha c || c == '_', reserved = Set.empty }
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = lazy (\_ -> expression)
            , trailing = Optional
            }


type Expression
    = EFloat Float
    | ERange Range
    | ECoord Position
    | EApplication Application


expression : Parser Expression
expression =
    oneOf
        [ map EFloat float
        , Position.parser |> andThen positionOrRange
        , map ECoord Position.parser
        , map EApplication application
        ]


type Contents
    = Expr Expression
    | Text String


contents : Parser Contents
contents =
    succeed identity
        |= oneOf
            [ succeed Expr |. symbol "=" |= expression
            , succeed Text |= (getChompedString <| chompWhile (always True))
            ]
        |. end


parseContents : String -> Result (List Parser.DeadEnd) Contents
parseContents input =
    run contents input
