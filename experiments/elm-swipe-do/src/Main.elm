module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (..)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] [ viewToolbar ]


viewToolbar =
    div [ class "flex w-100 justify-center bg-black white" ]
        [ div [ class "flex w-100 measure-wide items-center" ]
            [ txtC "b ph3" "ELM Swipe Do"
            , spacer
            , viewTabs
            , spacer
            ]
        ]


viewTabs =
    div [ class "flex bw2 bt b--transparent" ]
        [ txtC "b ph3 pv2" "Scheduled"
        , txtC "b ph3 pv2 bw2 bb b--blue" "Todo"
        , txtC "b ph3 pv2" "Done"
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
