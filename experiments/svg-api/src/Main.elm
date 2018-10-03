module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, height, src, style, width)
import Html.Lazy
import ISvg exposing (..)
import Json.Decode as D
import Json.Encode as E
import Ramda exposing (ensureAtLeast, ifElse, mapT, scale, subBy, ter)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as TP


---- MODEL ----


type alias Ball =
    { pos : ( Float, Float ), r : Float }


type alias Model =
    { ball : Ball, keySet : Set String }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { ball = Ball ( 100, 100 ) 15
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


getArrowKeyXYDirection m =
    let
        xDirection =
            ter (isLeftDown m) -1 0
                + ter (isRightDown m) 1 0

        yDirection =
            ter (isUpDown m) -1 0
                + ter (isDownDown m) 1 0
    in
        ( xDirection, yDirection )


getWorldDimension m =
    ( 600, 350 )



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame Float
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

                newBall =
                    { ball | pos = computeNewBallPos delta m }

                {- _ =
                   Debug.log "delta" delta
                -}
            in
                { m | ball = newBall } |> pure

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> pure

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> pure


computeNewBallPos delta m =
    let
        ballSpeedInPxPerSecond =
            200

        ballVelocity =
            getArrowKeyXYDirection m
                |> mapT (ballSpeedInPxPerSecond * delta |> (*))

        ( ww, wh ) =
            getWorldDimension m

        r =
            m.ball.r

        newPos =
            addVec m.ball.pos ballVelocity
                |> Tuple.mapBoth (clamp r (ww - r)) (clamp r (wh - r))
    in
        newPos


pure model =
    ( model, Cmd.none )


addCmd c2 =
    Tuple.mapSecond (\c1 -> Cmd.batch [ c1, c2 ])


withCmd c m =
    pure m |> addCmd c


addVec ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )



---- VIEW ----


view : Model -> Html Msg
view m =
    div [ class "flex flex-column items-center pa2 h-100 " ]
        [ div [ class "flex flex-column vs3" ]
            [ div [ class "f3" ] [ text "SVG API" ]
            , viewSvg m
            ]
        ]


viewSvg m =
    let
        ( w, h ) =
            getWorldDimension m
    in
        Svg.svg
            [ width w
            , height h
            ]
            [ Svg.rect
                [ SA.width "100%"
                , SA.height "100%"
                , SA.strokeWidth "0.2"
                , SA.stroke "#000"
                , SA.fill "lightblue"
                ]
                []
            , viewBall m.ball
            ]


cPosR ( x, y ) r =
    [ TP.cx x, TP.cy y, TP.r r ]


viewBall : Ball -> Svg msg
viewBall { pos, r } =
    Svg.g []
        [ Svg.circle (cPosR pos r ++ [ SA.fill "blue", SA.opacity "0.6" ]) []
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationFrame
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
