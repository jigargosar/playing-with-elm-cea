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
    { vw : Int, vh : Int, ball : Ball, keySet : Set String, worldDimension : ( Float, Float ) }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { vw = vw
      , vh = vh
      , ball = Ball ( 100, 100 ) 20
      , keySet = Set.empty
      , worldDimension = ( 1024, 1024 )
      }
    , Cmd.batch [ upateWorldDimensionCmd ]
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
            ter (isLeftDown m) -1 0 + ter (isRightDown m) 1 0

        yDirection =
            ter (isUpDown m) -1 0 + ter (isDownDown m) 1 0
    in
        ( xDirection, yDirection )



---- UPDATE ----


type Msg
    = NoOp
    | Visibility Browser.Events.Visibility
    | AnimationFrame Float
    | Resize Int Int
    | KeyDown String
    | KeyUp String
    | WorldElement (Result Browser.Dom.Error Browser.Dom.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        WorldElement (Ok el) ->
            let
                { width, height } =
                    el.element
            in
                { m | worldDimension = ( width, height ) } |> pure

        WorldElement (Err err) ->
            pure m

        Visibility v ->
            let
                _ =
                    Debug.log "vvv" v
            in
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

        Resize nw nh ->
            { m | vw = nw, vh = nh }
                |> withCmd upateWorldDimensionCmd

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> pure

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> pure


upateWorldDimensionCmd =
    Browser.Dom.getElement "svgView"
        |> Task.attempt WorldElement


computeNewBallPos delta m =
    let
        ballSpeedInPxPerSecond =
            200

        ballVelocity =
            getArrowKeyXYDirection m
                |> mapT (ballSpeedInPxPerSecond * delta |> (*))

        ( ww, wh ) =
            m.worldDimension

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
    div [ class "measure-wide center pa2 h-100 flex flex-column vs3 " ]
        [ div [ class "f3" ] [ text "SVG API" ]
        , Svg.svg
            [ SA.width "100%"
            , SA.height "100%"
            , SA.id "svgView"
            ]
            [ Svg.rect
                [ SA.width "100%"
                , SA.height "100%"
                , SA.strokeWidth "0.5"
                , SA.stroke "#000"
                , SA.fill "none"
                ]
                []
            , viewWorldBoundary m.worldDimension
            , viewBall m.ball
            ]
        ]


cPosR ( x, y ) r =
    [ TP.cx x, TP.cy y, TP.r r ]


viewWorldBoundary ( width, height ) =
    Svg.rect
        [ TP.width width
        , TP.height height

        --        , SA.strokeWidth "0"
        --        , SA.stroke "#000"
        , SA.fill "lightblue"
        ]
        []


viewBall : Ball -> Svg msg
viewBall { pos, r } =
    Svg.g []
        [ Svg.circle (cPosR pos r ++ [ SA.fill "blue", SA.opacity "0.6" ]) []
        ]



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationFrame
        , Browser.Events.onResize Resize
        , Browser.Events.onVisibilityChange Visibility
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
