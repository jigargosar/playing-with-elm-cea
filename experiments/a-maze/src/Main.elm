module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class, src)
import Random


---- MODEL ----


type alias Model =
    {seed:Random.Seed}


type alias Flags =
    { now : Int }

init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( {seed=Random.initialSeed now}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
            [ div [ class "pa3 vs3" ]
                [ div [class "f2" ] [ text "A-Maze"]
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
