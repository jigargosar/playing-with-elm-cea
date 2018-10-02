module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class, src)
import Html.Lazy
import Svg


---- MODEL ----


type alias Model =
    {}


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "pa3 flex flex-column vs3"]
        [ div[class "f3"][text "SVG API"]
        , Svg.svg [] []
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta (\_ -> AnimationFrame) ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.Lazy.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
