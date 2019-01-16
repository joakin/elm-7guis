module Ui exposing (box, column, px, row, spacer)

import Html exposing (..)
import Html.Attributes exposing (..)


px : Int -> String
px n =
    String.fromInt n ++ "px"


row attrs children =
    div ([ style "display" "flex", style "flex-direction" "row" ] ++ attrs) children


column attrs children =
    div ([ style "display" "flex", style "flex-direction" "column" ] ++ attrs) children


box attrs children =
    div (style "padding" "0.5rem" :: attrs) children


spacer =
    div [ style "padding" "0.25rem" ] []
