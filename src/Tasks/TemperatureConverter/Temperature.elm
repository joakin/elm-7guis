module Tasks.TemperatureConverter.Temperature exposing
    ( Temperature
    , fromCelsius
    , fromFarenheit
    , toCelsius
    , toFarenheit
    )


type Temperature
    = InCelsius Float


fromCelsius : Float -> Temperature
fromCelsius n =
    InCelsius n


toCelsius : Temperature -> Float
toCelsius (InCelsius n) =
    n


fromFarenheit : Float -> Temperature
fromFarenheit n =
    InCelsius <| (n - 32) * (5 / 9)


toFarenheit : Temperature -> Float
toFarenheit (InCelsius n) =
    n * (9 / 5) + 32
