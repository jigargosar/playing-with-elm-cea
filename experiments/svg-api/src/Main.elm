module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Lazy
import Svg



---- MODEL ----


type alias Model =
    { vw : Int, vh : Int }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { vw = vw, vh = vh }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame
    | Resize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        AnimationFrame ->
            pure m

        Resize nw nh ->
            { m | vw = nw, vh = nh } |> Debug.log "winDim" |> pure


pure model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "measure-wide center pa3 flex flex-column vs3 " ]
        [ div [ class "f3" ] [ text "SVG API" ]
        , Svg.svg [] []
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\_ -> AnimationFrame)
        , Browser.Events.onResize Resize
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.Lazy.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
