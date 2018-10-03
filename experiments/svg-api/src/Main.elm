module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Lazy
import ISvg exposing (..)
import Json.Decode as D
import Json.Encode as E
import Ramda exposing (ifElse, subBy, ter)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as TP


---- MODEL ----


type alias Ball =
    { x : Float, y : Float, r : Int }


type alias Model =
    { vw : Int, vh : Int, ball : Ball, keySet : Set String }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { vw = vw
      , vh = vh
      , ball = Ball 100 100 20
      , keySet = Set.empty
      }
    , Cmd.none
    )


isKeyDown : String -> Model -> Bool
isKeyDown key =
    .keySet >> Set.member key


isLeftDown : Model -> Bool
isLeftDown =
    isKeyDown "ArrowLeft"


isRightDown : Model -> Bool
isRightDown =
    isKeyDown "ArrowRight"


isUpDown : Model -> Bool
isUpDown =
    isKeyDown "ArrowUp"


isDownDown : Model -> Bool
isDownDown =
    isKeyDown "ArrowDown"



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame Float
    | Resize Int Int
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        AnimationFrame elapsed ->
            let
                delta =
                    elapsed / 1000

                ball =
                    m.ball

                xDirection =
                    ter (isLeftDown m) (-1) (ter (isRightDown m) (1) (0))

                yDirection =
                    ter (isUpDown m) (-1) (ter (isDownDown m) (1) (0))

                ballSpeedPerSecond =
                    60

                xOffset =
                    xDirection * ballSpeedPerSecond * delta

                yOffset =
                    yDirection * ballSpeedPerSecond * delta

                newBall =
                    { ball | x = ball.x + xOffset, y = ball.y + yOffset }

                {- _ =
                   Debug.log "delta" delta
                -}
            in
                { m | ball = newBall } |> pure

        Resize nw nh ->
            { m | vw = nw, vh = nh } |> Debug.log "[Resize] model:" |> pure

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> pure

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> pure


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


viewBall : Ball -> Svg msg
viewBall { x, y, r } =
    Svg.g []
        [ Svg.circle [ TP.cx x, TP.cy y, iR r, SA.fill "blue", SA.opacity "0.6" ] []
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationFrame
        , Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (D.map KeyDown (D.field "key" D.string))
        , Browser.Events.onKeyUp (D.map KeyUp (D.field "key" D.string))
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.Lazy.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
