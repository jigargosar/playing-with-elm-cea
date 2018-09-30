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


view worldRect maze =
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
        [ viewAxis worldRect, viewMaze maze ]
    ]


viewMaze maze =
    let
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
                        drawWithFill fillS =
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

                        drawPath =
                            drawWithFill "#cd37a9"

                        drawWall =
                            drawWithFill "#000"
                    in
                    case maze.data |> Array2D.get (round y) (round x) of
                        Nothing ->
                            drawWall

                        Just { down, right } ->
                            let
                                isSouthWallCord =
                                    y >= pathSize

                                isRightWallCord =
                                    x >= pathSize

                                shouldDrawWall =
                                    (isSouthWallCord && down)
                                        || (isRightWallCord && right)
                            in
                            ter shouldDrawWall
                                drawWall
                                drawPath
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
    mapCoordinates2D maze.width maze.height drawMazeCellAt
        |> g []
