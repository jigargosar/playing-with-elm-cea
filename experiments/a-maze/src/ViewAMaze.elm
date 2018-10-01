module ViewAMaze exposing (view)

import AMaze
import Frame2d
import Html.Lazy
import ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)
import Point2d
import Ramda exposing (mapCoordinates2D, ter)
import Rectangle2d
import Round
import Svg exposing (g, line, rect)
import Svg.Attributes as SA


view maze =
    [ rect
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "#fff"
        , SA.stroke "#cd37a9"
        , SA.strokeWidth "2"
        , SA.opacity "0.4"
        ]
        []
    , g [ SA.transform (iTranslate 20 20) ] [ viewMaze maze ]
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
        dataAt x y =
            maze |> AMaze.dataAt x y
    in
    mapCoordinates2D
        maze.width
        maze.height
        (drawMazeCellAt dataAt)
        |> g []


drawInnerCell x y fillS =
    rect
        [ x * mazeInnerCellSizeInPx |> iX
        , y * mazeInnerCellSizeInPx |> iY
        , iWidth mazeInnerCellSizeInPx
        , iHeight mazeInnerCellSizeInPx
        , SA.fill fillS
        ]
        []


isSouthWallCord x y =
    y == 2


isRightWallCord x y =
    x == 2


drawMazeCellAt dataAt cellX cellY =
    let
        cellData =
            dataAt cellX cellY

        drawInnerGridCell x y =
            let
                drawPath =
                    drawInnerCell x y "#cd37a9"

                drawWall =
                    drawInnerCell x y "#000"
            in
            case cellData of
                Nothing ->
                    drawWall

                Just { down, right } ->
                    let
                        shouldDrawWall =
                            (isSouthWallCord x y && down)
                                || (isRightWallCord x y && right)
                    in
                    ter shouldDrawWall drawWall drawPath
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



---- SVG INT HELPERS ----
