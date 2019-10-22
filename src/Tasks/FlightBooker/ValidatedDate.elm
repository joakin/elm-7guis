module Tasks.FlightBooker.ValidatedDate exposing
    ( ValidatedDate
    , fromString
    , isEarlierThan
    , isValid
    , toString
    )

import Regex exposing (Regex)


type ValidatedDate
    = ValidatedDate Status String


type Status
    = Valid ( Int, Int, Int )
    | Invalid


fromString : String -> ValidatedDate
fromString value =
    ValidatedDate
        (parse value)
        value


parse : String -> Status
parse value =
    case Regex.find dateRegex value of
        [ match ] ->
            case match.submatches of
                [ Just dayS, Just monthS, Just yearS ] ->
                    Maybe.map3
                        (\day month year ->
                            Valid ( day, month, year )
                        )
                        (String.toInt dayS)
                        (String.toInt monthS)
                        (String.toInt yearS)
                        |> Maybe.withDefault Invalid

                _ ->
                    Invalid

        _ ->
            Invalid


dateRegex : Regex
dateRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(\\d+)\\.(\\d+).(\\d{4})$"


isValid : ValidatedDate -> Bool
isValid (ValidatedDate date str) =
    case date of
        Valid _ ->
            True

        Invalid ->
            False


isEarlierThan : ValidatedDate -> ValidatedDate -> Bool
isEarlierThan (ValidatedDate later _) (ValidatedDate earlier _) =
    case ( earlier, later ) of
        ( Valid ( d1, m1, y1 ), Valid ( d2, m2, y2 ) ) ->
            if y1 < y2 then
                True

            else if y1 > y2 then
                False

            else if m1 < m2 then
                True

            else if m1 > m2 then
                False

            else if d1 < d2 then
                True

            else
                False

        _ ->
            False


toString : ValidatedDate -> String
toString (ValidatedDate date str) =
    str
