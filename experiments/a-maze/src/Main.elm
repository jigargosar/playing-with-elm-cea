module Main exposing (Flags, Model, Msg(..), init, main, update, view, worldRect)

import AMaze
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick)
import Point2d
import Random
import Rectangle2d
import Svg.Attributes
import TypedSvg exposing (svg)



---- MODEL ----


type alias Model =
    { seed : Random.Seed }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( { seed = Random.initialSeed now }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


worldRect =
    Rectangle2d.from Point2d.origin (Point2d.fromCoordinates ( 500, 500 ))


view : Model -> Html Msg
view model =
    let
        ( worldWidth, worldHeight ) =
            Rectangle2d.dimensions worldRect
    in
    div []
        [ div [ class "pa3 vs3" ]
            [ div [ class "f2" ] [ text "A-Maze" ]
            , div [ class "no-sel" ]
                [ svg
                    [ Svg.Attributes.class "flex center"
                    , width (worldWidth |> round)
                    , height (worldHeight |> round)
                    ]
                    (AMaze.view worldRect)
                ]
            ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
