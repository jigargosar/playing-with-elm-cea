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
        , SA.opacity "0.4"
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
            30

        wallSize =
            2

        topWall =
            rect
                [ x 0
                , y 0
                , width cellSize
                , height wallSize
                , SA.fill "#000"
                , SA.strokeWidth "0"
                , SA.stroke "#cd37a9"
                ]
                []

        bottomWall =
            g [ transform [ Rotate 180 (cellSize / 2) (cellSize / 2) ] ] [ topWall ]

        rightWall =
            g [ transform [ Rotate 90 (cellSize / 2) (cellSize / 2) ] ] [ topWall ]

        leftWall =
            g [ transform [ Rotate -90 (cellSize / 2) (cellSize / 2) ] ] [ topWall ]

        viewCell ( x_, y_ ) walls =
            g
                [ transform
                    [ Translate (x_ * cellSize) (y_ * cellSize)
                    ]
                ]
                ([ rect
                    [ width cellSize
                    , height cellSize
                    , SA.fill "#cd37a9"
                    , SA.strokeWidth "0"
                    , SA.stroke "#fff"
                    ]
                    []
                 ]
                    ++ walls
                )
    in
    g []
        [ rect
            [ width (cellSize * 3)
            , height (cellSize * 2)
            , SA.fill "#cd37a9"

            --            , strokeWidth (wallSize * 4)
            , SA.stroke "#000"
            ]
            []
        , viewCell ( 0, 0 ) [ bottomWall ]
        , viewCell ( 0, 1 ) [ topWall ]
        , viewCell ( 1, 0 ) []
        , viewCell ( 1, 1 ) [ rightWall ]
        , viewCell ( 2, 0 ) []
        , viewCell ( 2, 1 ) [ leftWall ]
        ]
