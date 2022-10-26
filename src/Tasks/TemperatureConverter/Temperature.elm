module Tasks.TemperatureConverter.Temperature exposing
    ( Temperature
    , fromCelsius
    , fromFarenheit
    , toCelsius
    , toFarenheit
    )

{-
   Store the temperature in whatever unit Float was given, otherwise floating
   point precission loss can lead to errors.
-}


type Temperature
    = InCelsius Float
    | InFarenheit Float


fromCelsius : Float -> Temperature
fromCelsius n =
    InCelsius n


toCelsius : Temperature -> Float
toCelsius t =
    case t of
        InCelsius n ->
            n

        InFarenheit n ->
            (n - 32) * (5 / 9)


fromFarenheit : Float -> Temperature
fromFarenheit n =
    InFarenheit n


toFarenheit : Temperature -> Float
toFarenheit t =
    case t of
        InCelsius n ->
            n * (9 / 5) + 32

        InFarenheit n ->
            n
