module ViewSvg exposing (view)

import Particle
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
import Vec


viewBall ball =
    let
        ( bx, by ) =
            Particle.posPair ball

        ballRadius =
            Particle.getR ball
    in
    circle
        [ cx bx
        , cy by
        , r ballRadius
        , SA.fill "#cd37a9"
        , opacity (Opacity 0.8)
        ]
        []


viewAxis worldSize =
    let
        ( ww, wh ) =
            Vec.toPair worldSize
    in
    g
        [ SA.stroke "#cd37a9"
        , SA.strokeWidth "2"
        , opacity (Opacity 0)
        , strokeLinecap StrokeLinecapRound
        ]
        [ line
            [ y1 (wh / -2)
            , y2 (wh / 2)
            ]
            []
        , line
            [ x1 (ww / -2)
            , x2 (ww / 2)
            ]
            []
        , line
            [ x1 100
            , y1 100
            ]
            []
        , line
            [ x1 -100
            , y1 100
            ]
            []
        ]


view { balls, worldSize, ship } =
    let
        ( ox, oy ) =
            worldSize |> Vec.div 2 |> Vec.toPair
    in
    [ g []
        [ rect
            [ SA.width "100%"
            , SA.height "100%"
            , SA.fill "#adbeeb"
            , SA.fill "#fff"
            , SA.stroke "#cd37a9"
            , SA.strokeWidth "2"
            , opacity (Opacity 0.1)
            , strokeLinecap StrokeLinecapRound
            ]
            []
        , g
            [ transform [ Translate ox oy, Scale 1 -1 ]
            ]
            [ viewAxis worldSize
            , g [] (balls |> List.map viewBall)
            , viewShip ship
            ]
        ]
    ]


viewShip ship =
    let
        ( bx, by ) =
            Particle.posPair ship

        ra =
            Particle.getR ship

        ang =
            Particle.getA ship
    in
    polygon
        [ points [ ( 0, 0 ), ( -ra, ra / -3 ), ( -ra, ra / 3 ) ]
        , SA.stroke "#cd37a9"
        , strokeWidth 5
        , fill FillNone
        , strokeLinecap StrokeLinecapRound
        , strokeLinejoin StrokeLinejoinMiter
        , strokeLinejoin StrokeLinejoinRound
        , strokeLinejoin StrokeLinejoinBevel
        , opacity (Opacity 0.8)
        , transform [ Rotate ang 0 0 ]
        ]
        []
