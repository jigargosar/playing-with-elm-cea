module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import BoundingBox2d
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


type alias Position =
    ( Float, Float )


type alias Dimension =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Wall =
    { pos : Position, size : Dimension }


type alias Ball =
    { pos : ( Float, Float ), r : Float, vel : Velocity }


type alias Model =
    { ball : Ball
    , walls : List Wall
    , keySet : Set String
    }


type alias Flags =
    { now : Int, vw : Int, vh : Int }


init : Flags -> ( Model, Cmd Msg )
init { now, vw, vh } =
    ( { ball = Ball ( 100, 100 ) 15 ( 0, 0 )
      , keySet = Set.empty
      , walls = [ Wall ( 0, 0 ) ( Tuple.first worldDimension, 10 ) ]
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


worldDimension =
    ( 600, 350 )


getWorldDimension m =
    worldDimension



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

                newVel =
                    computeBallVelocity delta m

                ball =
                    setBallVelocity newVel m.ball

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


type alias EX =
    { minX : Float, maxX : Float, minY : Float, maxY : Float }


posSizeToRecord { pos, size } =
    let
        ( x, y ) =
            pos

        ( w, h ) =
            size
    in
        { x = x, y = y, w = w, h = h }


posSizeToExtrema { pos, size } =
    let
        ( x, y ) =
            pos

        ( w, h ) =
            size
    in
        EX x (x + w) y (y + h)


intersects posSize1 posSize2 =
    BoundingBox2d.intersects
        (posSizeToExtrema posSize1 |> BoundingBox2d.fromExtrema)
        (posSizeToExtrema posSize2 |> BoundingBox2d.fromExtrema)


getBallPosSize { pos, r } =
    { pos = pos |> addVec ( -r, -r ), size = ( r * 2, r * 2 ) }


computeBallVelocity delta m =
    let
        ballSpeedInPxPerSecond =
            60
    in
        getArrowKeyXYDirection m
            |> mapT (ballSpeedInPxPerSecond * delta |> (*))


setBallVelocity vel ball =
    { ball | vel = vel }


computeNewBallPos delta m =
    let
        ball =
            m.ball

        ballVelocity =
            ball.vel

        newPos =
            addVec ball.pos ball.vel

        ballBB =
            getBallPosSize { pos = newPos, r = ball.r }

        collided =
            m.walls |> List.any (intersects ballBB)
    in
        ter collided ball.pos newPos


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
            , viewWalls m.walls
            ]


cPosR ( x, y ) r =
    [ TP.cx x, TP.cy y, TP.r r ]


rectAttrFromBB { pos, size } =
    let
        ( x, y ) =
            pos

        ( w, h ) =
            size
    in
        [ TP.x x, TP.y y, TP.width w, TP.height h ]


viewBall : Ball -> Svg msg
viewBall ball =
    let
        { pos, r } =
            ball

        ballBB =
            getBallPosSize ball
    in
        Svg.g []
            [ Svg.circle (cPosR pos r ++ [ SA.fill "blue", SA.opacity "0.6" ]) []

            {- , Svg.rect
               (rectAttrFromBB ballBB
                   ++ [ SA.fill "none"
                      , SA.stroke "red"
                      , SA.opacity "0.6"
                      ]
               )
               []
            -}
            ]


viewWalls : List Wall -> Svg msg
viewWalls walls =
    Svg.g [] (walls |> List.map viewWall)


viewWall : Wall -> Svg msg
viewWall wall =
    Svg.g []
        [ Svg.rect
            (rectAttrFromBB wall
                ++ [ SA.fill "darkblue"
                   , SA.opacity "0.8"
                   ]
            )
            []

        {- , Svg.rect
           (rectAttrFromBB wall
               ++ [ SA.fill "none"
                  , SA.stroke "red"
                  , SA.opacity "0.6"
                  ]
           )
           []
        -}
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
