module ViewAMaze exposing (view)

import AMaze
import Frame2d
import Html.Lazy
import Point2d
import Ramda exposing (mapCoordinates2D, ter)
import Rectangle2d
import Round
import Svg exposing (g, line, rect)
import Svg.Attributes as SA
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (Transform(..))


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
    , g [ transform [ Translate 20 20 ] ] [ viewMaze maze ]
    ]


pathSize =
    2


wallSize =
    1


mazeCellSize =
    pathSize + wallSize


mazeInnerCellSizeInPx =
    10


mazeCellSizeInPx =
    mazeInnerCellSizeInPx * mazeCellSize


viewMaze maze =
    let
        drawMazeCellAt cellX cellY =
            let
                drawInnerGridCell x y =
                    let
                        drawWithFill fillS =
                            rect
                                [ (x * mazeInnerCellSizeInPx)
                                    |> String.fromInt
                                    |> SA.x
                                , (y * mazeInnerCellSizeInPx)
                                    |> String.fromInt
                                    |> SA.y
                                , SA.width (mazeInnerCellSizeInPx |> String.fromInt)
                                , SA.height (mazeInnerCellSizeInPx |> String.fromInt)
                                , SA.fill fillS
                                , SA.strokeWidth "0"
                                , SA.stroke "#000"
                                ]
                                []

                        drawPath =
                            drawWithFill "#cd37a9"

                        drawWall =
                            drawWithFill "#000"
                    in
                    case maze |> AMaze.dataAt x y of
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
                            ter (isSouthWallCord || isRightWallCord) drawWall drawPath
            in
            g
                [ iTranslate
                    (cellX * mazeCellSizeInPx)
                    (cellY * mazeCellSizeInPx)
                    |> SA.transform
                ]
                (mapCoordinates2D
                    mazeCellSize
                    mazeCellSize
                    drawInnerGridCell
                )
    in
    mapCoordinates2D maze.width maze.height drawMazeCellAt
        |> g []


iTranslate x y =
    [ "translate("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ")"
    ]
        |> String.join ""
