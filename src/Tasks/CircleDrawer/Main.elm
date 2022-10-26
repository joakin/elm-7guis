module Tasks.CircleDrawer.Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Ui exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- TYPES


type alias Model =
    { circles : Circles
    , selectedCircle : Maybe Id
    , lastId : Int
    , history : List Circles
    , future : List Circles
    , modal : Maybe Modal
    }


type alias Circles =
    List ( Id, Circle )


type alias Circle =
    { x : Int, y : Int, radius : Int }


type alias Id =
    Int


type alias Modal =
    Int


type Msg
    = CanvasClick ClickInfo
    | CircleClick Id
    | RadiusInput Id String
    | Undo
    | Redo


type alias ClickInfo =
    { pagePos : Position, offsetPos : { left : Int, top : Int } }


type alias Position =
    { x : Int, y : Int }



-- UPDATE


init : Model
init =
    { circles = []
    , selectedCircle = Nothing
    , modal = Nothing
    , lastId = 0
    , history = []
    , future = []
    }
        |> saveHistory


update : Msg -> Model -> Model
update msg ({ selectedCircle, circles } as model) =
    case msg of
        CanvasClick { offsetPos, pagePos } ->
            case selectedCircle of
                Just _ ->
                    clearSelectionAndModal model

                Nothing ->
                    let
                        pos =
                            { x = pagePos.x - offsetPos.left
                            , y = pagePos.y - offsetPos.top
                            }
                    in
                    addCircle pos model
                        |> saveHistory

        CircleClick id ->
            case getCircle id circles of
                Just circle ->
                    { model
                        | selectedCircle = Just id
                        , modal = Just circle.radius
                    }
                        |> saveHistory

                Nothing ->
                    model

        RadiusInput id str ->
            case ( model.modal, String.toInt str ) of
                ( Just _, Just radius ) ->
                    { model
                        | circles = updateCircleRadius id radius circles
                        , modal = Just radius
                    }
                        |> replaceHistory

                _ ->
                    model

        Undo ->
            undo model
                |> clearSelectionAndModal

        Redo ->
            redo model
                |> clearSelectionAndModal


clearSelectionAndModal : Model -> Model
clearSelectionAndModal model =
    { model
        | selectedCircle = Nothing
        , modal = Nothing
    }



-- HISTORY


saveHistory : Model -> Model
saveHistory model =
    { model
        | history = model.circles :: model.history
        , future = []
    }


replaceHistory : Model -> Model
replaceHistory model =
    case model.history of
        _ :: hs ->
            { model
                | history = model.circles :: hs
                , future = []
            }

        [] ->
            -- History should never be empty, there should always be an initial
            -- state
            model


undo : Model -> Model
undo model =
    case model.history of
        cs :: [] ->
            if model.circles /= cs then
                -- Initial history state, restore but don't delete history
                { model
                    | circles = cs
                    , future = model.circles :: model.future
                }

            else
                model

        cs :: hs ->
            -- Undo only if current state is different than last state
            if model.circles /= cs then
                { model
                    | history = hs
                    , future = model.circles :: model.future
                    , circles = cs
                }

            else
                -- Skip state if it is the same as the current one
                undo { model | history = hs }

        [] ->
            model


redo : Model -> Model
redo model =
    case model.future of
        f :: fs ->
            { model
                | history = model.circles :: model.history
                , future = fs
                , circles = f
            }

        [] ->
            model



-- CIRCLES


addCircle : Position -> Model -> Model
addCircle { x, y } ({ circles, lastId } as model) =
    { model
        | circles = circles ++ [ ( lastId, { x = x, y = y, radius = 10 } ) ]
        , lastId = lastId + 1
    }


getCircle : Id -> Circles -> Maybe Circle
getCircle id circles =
    circles
        |> List.filter (\( id_, _ ) -> id == id_)
        |> List.map (\( _, circle ) -> circle)
        |> List.head


updateCircleRadius : Id -> Int -> Circles -> Circles
updateCircleRadius id radius circles =
    circles
        |> List.map
            (\( id_, circle ) ->
                if id == id_ then
                    ( id_, { circle | radius = radius } )

                else
                    ( id_, circle )
            )



-- VIEWS


view : Model -> Html Msg
view { circles, selectedCircle, modal } =
    column []
        [ row [ style "justify-content" "center" ]
            [ button [ onClick Undo ] [ text "Undo" ]
            , spacer
            , spacer
            , button [ onClick Redo ] [ text "Redo" ]
            ]
        , spacer
        , Keyed.node "div"
            [ style "width" "500px"
            , style "height" "500px"
            , style "border" "1px solid black"
            , style "position" "relative"
            , style "overflow" "hidden"
            , on "click" (clickDecoder CanvasClick)
            ]
          <|
            List.map (viewCircle selectedCircle) circles
        , viewModal selectedCircle modal
        ]


viewCircle : Maybe Id -> ( Id, Circle ) -> ( String, Html Msg )
viewCircle selectedCircle ( id, { x, y, radius } ) =
    let
        selected =
            selectedCircle
                |> Maybe.map (\id_ -> id == id_)
                |> Maybe.withDefault False
    in
    ( String.fromInt id
    , div
        [ style "position" "absolute"
        , style "left" <| px x
        , style "top" <| px y
        , style "width" <| px (radius * 2)
        , style "height" <| px (radius * 2)
        , style "transform" <|
            "translateX(-"
                ++ px radius
                ++ ")"
                ++ "translateY(-"
                ++ px radius
                ++ ")"
        , style "border-radius" "50%"
        , style "border" "1px solid black"
        , style "background-color"
            (if selected then
                "lightgray"

             else
                "white"
            )
        , stopPropagationOn "click"
            (Decode.succeed (CircleClick id)
                |> Decode.map (\msg -> ( msg, True ))
            )
        ]
        []
    )


viewModal : Maybe Id -> Maybe Modal -> Html Msg
viewModal maybeId maybeModal =
    let
        viewM attrs inputAttrs radius =
            box attrs
                [ row
                    [ style "justify-content" "space-evenly"
                    ]
                    [ span [] [ text "Choose circle size" ]
                    , input
                        ([ type_ "range"
                         , Attributes.min "2"
                         , Attributes.max
                            "100"
                         , step "1"
                         , value (String.fromInt radius)
                         ]
                            ++ inputAttrs
                        )
                        []
                    ]
                ]
    in
    case ( maybeId, maybeModal ) of
        ( Just id, Just radius ) ->
            viewM [] [ onInput <| RadiusInput id ] radius

        _ ->
            viewM [ style "opacity" "0" ] [] 0



-- DECODERS


clickDecoder msg =
    Decode.map2 (\pagePos offsetPos -> msg { pagePos = pagePos, offsetPos = offsetPos })
        clientDecoder
        offsetDecoder


clientDecoder =
    Decode.map2 (\x y -> { x = x, y = y })
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


offsetDecoder =
    Decode.field "target"
        (Decode.map2 (\top left -> { left = left, top = top })
            (Decode.field "offsetTop" Decode.int)
            (Decode.field "offsetLeft" Decode.int)
        )
