module Tasks.FlightBooker.FlightKind exposing
    ( FlightKind(..)
    , fromString
    , toString
    )


type FlightKind
    = OneWay
    | Return


toString : FlightKind -> String
toString kind =
    case kind of
        OneWay ->
            "one-way flight"

        Return ->
            "return flight"


fromString : String -> FlightKind
fromString kind =
    case kind of
        "one-way flight" ->
            OneWay

        "return flight" ->
            Return

        _ ->
            OneWay
