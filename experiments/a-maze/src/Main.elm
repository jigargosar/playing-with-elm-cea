module Main exposing (Flags, Model, Msg(..), init, main, update, view, worldRect)

import AMaze exposing (Maze, mazeHeight, mazeWidth)
import Array2D
import Browser
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick)
import Point2d
import Random
import Random.Array
import Random.Extra
import Rectangle2d
import Svg.Attributes
import TypedSvg exposing (svg)
import ViewAMaze



---- MODEL ----


type alias Model =
    { seed : Random.Seed, maze : Maze }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( mazeDataList2D, seed ) =
            Random.step AMaze.dataGenerator initialSeed
    in
    ( { seed = seed
      , maze =
            { width = mazeWidth
            , height = mazeHeight
            , data = Array2D.fromList mazeDataList2D
            }
      }
    , Cmd.none
    )



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
                    (ViewAMaze.view worldRect model.maze)
                ]
            ]
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta (\_ -> NoOp) ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
