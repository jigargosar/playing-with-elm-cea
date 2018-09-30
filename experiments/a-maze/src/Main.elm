module Main exposing (Flags, Model, Msg(..), init, main, update, view, worldRect)

import AMaze
import Array2D
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


mazeWidth =
    20


mazeHeight =
    25


type alias MazePath =
    { down : Bool, right : Bool }


type alias Maze =
    { width : Int, height : Int, data : Array2D.Array2D MazePath }


type alias Model =
    { seed : Random.Seed, maze : Maze }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        mazeData =
            Array2D.repeat mazeHeight mazeWidth (MazePath True False)
    in
    ( { seed = Random.initialSeed now, maze = Maze mazeWidth mazeHeight mazeData }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


worldRect =
    Rectangle2d.from Point2d.origin (Point2d.fromCoordinates ( 600, 550 ))


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
                    (AMaze.view worldRect model.maze)
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
