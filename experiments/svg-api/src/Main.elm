module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Lazy
import ISvg exposing (..)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Ball =
    { x : Int, y : Int, r : Int }


type alias Model =
    { vw : Int, vh : Int, ball : Ball }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { vw = vw, vh = vh, ball = Ball 100 100 20 }, Cmd.none )



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
view m =
    div [ class "measure-wide center pa2 h-100 flex flex-column vs3 " ]
        [ div [ class "f3" ] [ text "SVG API" ]
        , Svg.svg
            [ SA.width "100%"
            , SA.height "100%"
            ]
            [ Svg.rect
                [ SA.width "100%"
                , SA.height "100%"
                , SA.strokeWidth "0.5"
                , SA.stroke "#000"
                , SA.fill "none"
                ]
                []
            , viewBall m.ball
            ]
        ]


viewBall { x, y, r } =
    Svg.g []
        [ Svg.circle [ iCX x, iCY y, iR r, SA.fill "blue", SA.opacity "0.6" ] []
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
