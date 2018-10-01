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


type alias Model =
    { seed : Random.Seed, maze : AMaze }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( newMaze, newSeed ) =
            generateRandomMaze initialSeed
    in
    ( { seed = newSeed
      , maze = newMaze
      }
    , Cmd.none
    )


generateRandomMaze : Random.Seed -> ( AMaze, Random.Seed )
generateRandomMaze =
    Random.step (AMaze.randomGenerator 18 13)



---- UPDATE ----


type Msg
    = NoOp
    | New
    | Walled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        New ->
            updateGenerateNewMaze m |> pure

        Walled ->
            { m | maze = AMaze.fillWalls m.maze } |> pure


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
                , button [ onClick New ] [ text "New" ]
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
