module Main exposing (main)

import AMaze exposing (AMaze)
import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iHeight, iWidth)
import Random
import Random.Array
import Random.Extra
import Svg.Attributes as SA
import TypedSvg exposing (svg)
import ViewAMaze



---- MODEL ----


type alias Coordinate2D =
    ( Int, Int )


type alias Model =
    { seed : Random.Seed, maze : AMaze, stack : List Coordinate2D }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    update Walled
        { seed = Random.initialSeed now
        , maze = walledMaze
        , stack = []
        }


mazeWidth =
    18


mazeHeight =
    15


generateRandomMaze : Random.Seed -> ( AMaze, Random.Seed )
generateRandomMaze =
    Random.step (AMaze.randomGenerator mazeWidth mazeHeight)


walledMaze : AMaze
walledMaze =
    AMaze.walled mazeWidth mazeHeight



---- UPDATE ----


type Msg
    = NoOp
    | GenerateRandom
    | Walled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        GenerateRandom ->
            updateGenerateNewMaze m |> pure

        Walled ->
            { m | maze = AMaze.fillWalls m.maze, stack = [ ( 0, 0 ) ] } |> pure


pure model =
    ( model, Cmd.none )


updateGenerateNewMaze m =
    let
        ( newMaze, newSeed ) =
            generateRandomMaze m.seed
    in
    { m | seed = newSeed, maze = newMaze }



---- VIEW ----


worldWidth =
    600


worldHeight =
    550


view : Model -> Html Msg
view model =
    div []
        [ div [ class "pa3 vs3" ]
            [ div [ class "flex items-end hs3" ]
                [ div [ class "f2" ] [ text "A-Maze" ]
                , button [ onClick GenerateRandom ] [ text "Random" ]
                , button [ onClick Walled ] [ text "Walled" ]
                ]
            , div [ class "no-sel" ]
                [ svg
                    [ SA.class "flex center"
                    , iWidth worldWidth
                    , iHeight worldHeight
                    ]
                    (ViewAMaze.view model.maze)
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
