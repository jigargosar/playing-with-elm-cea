module ViewSvg exposing (svgView)

import Particle
import Svg.Attributes as SA
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes exposing (opacity, strokeLinecap, transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x1, x2, y1, y2)
import TypedSvg.Types exposing (Opacity(..), StrokeLinecap(..), Transform(..), num)
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
        ]
        []


svgView { ball, worldSize } =
    let
        ( bx, by ) =
            Particle.posPair ball

        ballRadius =
            Particle.getR ball

        ( ww, wh ) =
            Vec.toPair worldSize

        ( ox, oy ) =
            worldSize |> Vec.div 2 |> Vec.toPair
    in
    [ g []
        [ rect [ SA.width "100%", SA.height "100%", SA.fill "#adbeeb", SA.fill "#fff" ] []
        , g
            [ transform [ Translate ox oy, Scale 1 -1 ]
            , SA.fill "#cd37a9"
            ]
            [ g
                [ SA.stroke "#cd37a9"
                , SA.strokeWidth "2"
                , opacity (Opacity 0.5)
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
            , viewBall ball
            ]
        ]
    ]
