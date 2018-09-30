module AMaze exposing (view, viewAxis)

import Frame2d
import Point2d
import Rectangle2d
import Svg.Attributes as SA
import TypedSvg exposing (g, line, rect, svg)
import TypedSvg.Attributes exposing (strokeLinecap, transform)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Types exposing (StrokeLinecap(..), Transform(..))


viewAxis worldRect =
    let
        ( ww, wh ) =
            Rectangle2d.dimensions worldRect
    in
    g
        [ SA.stroke "#cd37a9"
        , strokeWidth 1
        , SA.opacity "0.1"
        , strokeLinecap StrokeLinecapRound
        ]
        [ line [ y1 (wh / -2), y2 (wh / 2) ] []
        , line [ x1 (ww / -2), x2 (ww / 2) ] []
        , line [ x1 100, y1 100 ] []
        , line [ x1 -100, y1 100 ] []
        ]


view worldRect =
    let
        ( tx, ty ) =
            Rectangle2d.centerPoint worldRect |> Point2d.coordinates
    in
    [ rect
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "#fff"
        , SA.stroke "#cd37a9"
        , SA.strokeWidth "2"
        , SA.opacity "0.1"
        ]
        []
    , g
        [ transform [ Translate tx ty ]
        ]
        [ viewAxis worldRect, viewGrid ]
    ]


viewGrid =
    let
        cellCount =
            10

        cellSize =
            50

        viewCell cords =
            let
                ss =
                    Rectangle2d.centeredOn
                        (Frame2d.atCoordinates cords)
                        ( cellSize, cellSize )

                ( xx, yy ) =
                    cords

                wallSize =
                    10

                hideLeftWall =
                    line
                        [ wallSize |> y1
                        , cellSize - wallSize |> y2
                        , cellSize / 2 |> x1
                        , cellSize / 2 |> x2
                        , SA.stroke "#cdf7a9"
                        , strokeLinecap StrokeLinecapSquare
                        , strokeWidth wallSize
                        , SA.opacity "1"
                        ]
                        []
            in
            g
                [ transform
                    [ Translate (xx * cellSize) (yy * cellSize)
                    ]
                ]
                [ rect
                    [ width cellSize
                    , height cellSize
                    , SA.fill "#cd37a9"
                    , SA.stroke "#000"
                    , strokeWidth wallSize
                    , SA.opacity "1"
                    ]
                    []
                , g [] [ hideLeftWall ]

                --                , g [ transform [ Translate (cellSize / -2) 0 ] ] [ hideLeftWall ]
                ]
    in
    g []
        [ viewCell ( 0, 0 )
        , viewCell ( 0, 1 )
        , viewCell ( 1, 0 )
        , viewCell ( 1, 1 )
        ]
