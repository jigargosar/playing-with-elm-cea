module ViewSvg exposing (view, viewBalls, viewParticle, viewShip)

import Math.Vector2
import Particle
import Ramda exposing (ter)
import Svg.Attributes as SA
import TypedSvg exposing (circle, g, line, polygon, rect, svg)
import TypedSvg.Attributes
    exposing
        ( fill
        , opacity
        , points
        , strokeLinecap
        , strokeLinejoin
        , transform
        )
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Types
    exposing
        ( Fill(..)
        , Opacity(..)
        , StrokeLinecap(..)
        , StrokeLinejoin(..)
        , Transform(..)
        , num
        )


roundToFloat =
    round >> toFloat


viewBall ball =
    let
        ( bx, by ) =
            Particle.getPosPair ball

        ballRadius =
            Particle.getR ball
    in
    circle
        [ cx (roundToFloat bx)
        , cy (roundToFloat by)
        , r ballRadius
        , SA.fill "#cd37a9"
        , opacity (Opacity 0.8)
        ]
        []


viewParticle particle fillColor =
    let
        ( bx, by ) =
            Particle.getPosPair particle

        ballRadius =
            Particle.getR particle
    in
    circle
        [ cx bx
        , cy by
        , r ballRadius
        , SA.fill fillColor
        , opacity (Opacity 1)
        ]
        []


viewAxis worldSize =
    let
        ( ww, wh ) =
            Math.Vector2.toRecord worldSize |> (\{ x, y } -> ( x, y ))
    in
    g
        [ SA.stroke "#cd37a9"
        , strokeWidth 1
        , opacity (Opacity 0.1)
        , strokeLinecap StrokeLinecapRound
        ]
        [ line [ y1 (wh / -2), y2 (wh / 2) ] []
        , line [ x1 (ww / -2), x2 (ww / 2) ] []
        , line [ x1 100, y1 100 ] []
        , line [ x1 -100, y1 100 ] []
        ]


view worldSize views =
    let
        origin =
            worldSize |> Math.Vector2.scale (1 / 2) >> Math.Vector2.toRecord
    in
    [ g []
        [ rect
            [ SA.width "100%"
            , SA.height "100%"
            , SA.fill "#fff"
            , SA.stroke "#cd37a9"
            , SA.strokeWidth "2"
            , opacity (Opacity 0.1)
            ]
            []
        , g
            [ transform [ Translate origin.x origin.y ]
            ]
            ([ viewAxis worldSize ] ++ views)
        ]
    ]


viewBalls balls =
    g [] (balls |> List.map viewBall)


viewShip ship shipAngle showThrust =
    let
        ( x, y ) =
            Particle.getPosPair ship

        ra =
            Particle.getR ship

        viewThrust =
            [ line
                [ x2 (-ra / 3)
                , SA.stroke "#cd37a9"
                , strokeWidth 5
                , strokeLinecapButt
                , opacity (Opacity 0.8)
                , transform [ Translate (-ra / 3) 0 ]
                ]
                []
            ]

        viewBody =
            polygon
                [ points [ ( ra / 3 * 2, 0 ), ( -ra / 3, -ra / 3 ), ( -ra / 3, ra / 3 ) ]
                , SA.stroke "#cd37a9"
                , strokeWidth 5
                , fill FillNone
                , strokeLinejoinBevel
                , opacity (Opacity 0.8)
                ]
                []
    in
    g [ transform [ Translate x y, Rotate shipAngle 0 0 ] ]
        ([ viewBody ] ++ ter showThrust viewThrust [])


strokeLinecapButt =
    strokeLinecap StrokeLinecapButt


strokeLinejoinBevel =
    strokeLinejoin StrokeLinejoinBevel
