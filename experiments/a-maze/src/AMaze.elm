module AMaze exposing (view, viewAxis)

import Array2D
import Frame2d
import Point2d
import Ramda exposing (mapCoordinates2D, ter)
import Rectangle2d
import Svg.Attributes as SA
import TypedSvg exposing (g, line, rect, svg)
import TypedSvg.Attributes exposing (fill, strokeLinecap, transform)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), Transform(..))


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
        [ {- transform [ Translate tx ty ], -} transform [ Translate 20 20 ]
        ]
        [ viewAxis worldRect, viewGrid ]
    ]


viewGrid1 =
    let
        cellSize =
            15

        nodeCellSize =
            1

        wallCellSize =
            1

        hNodes =
            9

        vNodes =
            8

        hGridCells =
            (hNodes * nodeCellSize) + (hNodes * wallCellSize - 1)

        vGridCells =
            (vNodes * nodeCellSize) + (vNodes * wallCellSize - 1)

        xCords =
            List.range 0 (hGridCells - 1)

        yCords =
            List.range 0 (vGridCells - 1)

        withXY x y =
            g [ transform [ Translate (x * cellSize) (y * cellSize) ] ]
                [ ter (modBy 2 (round x) == 0 && modBy 2 (round y) == 0) drawCell drawWall ]

        drawCell =
            rect
                [ width cellSize
                , height cellSize
                , SA.fill "#cd37a9"
                , strokeWidth 0
                , SA.stroke "#000"
                ]
                []

        drawWall =
            rect
                [ width cellSize
                , height cellSize
                , SA.fill "#000"
                , strokeWidth 0
                , SA.stroke "#fff"
                ]
                []
    in
    (xCords |> List.map (\x -> yCords |> List.map (\y -> withXY (toFloat x) (toFloat y))))
        |> List.concat
        |> g []


viewGrid =
    let
        _ =
            Array2D.repeat

        mazeWidth =
            20

        mazeHeight =
            25

        drawMazeCellAt cellX cellY =
            let
                pathSize =
                    8

                wallSize =
                    2

                mazeCellSize =
                    pathSize + wallSize

                mazeInnerCellSizeInPx =
                    2

                mazeCellSizeInPx =
                    mazeInnerCellSizeInPx * mazeCellSize

                drawInnerGridCell x y =
                    let
                        fillS =
                            ter (x >= pathSize || y >= pathSize)
                                "#000"
                                "#cd37a9"
                    in
                    rect
                        [ transform
                            [ Translate (x * mazeInnerCellSizeInPx)
                                (y * mazeInnerCellSizeInPx)
                            ]
                        , width mazeInnerCellSizeInPx
                        , height mazeInnerCellSizeInPx
                        , SA.fill fillS
                        , strokeWidth 0
                        , SA.stroke "#000"
                        ]
                        []
            in
            g
                [ transform
                    [ Translate
                        (cellX * mazeCellSizeInPx)
                        (cellY * mazeCellSizeInPx)
                    ]
                ]
                (mapCoordinates2D mazeCellSize mazeCellSize drawInnerGridCell)
    in
    mapCoordinates2D mazeWidth mazeHeight drawMazeCellAt
        |> g []
