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
    g [] (AMaze.mapData drawMazeCellAt maze)


drawInnerCell x y fillS =
    rect
        [ x * mazeInnerCellSizeInPx |> iX
        , y * mazeInnerCellSizeInPx |> iY
        , iWidth mazeInnerCellSizeInPx
        , iHeight mazeInnerCellSizeInPx
        , SA.fill fillS
        ]
        []


drawMazeCellAt cellX cellY cellData =
    g
        [ iTranslate
            (cellX * mazeCellSizeInPx)
            (cellY * mazeCellSizeInPx)
            |> SA.transform
        ]
        (mapCoordinates2D
            mazeCellSize
            mazeCellSize
            (drawInnerGridCell cellData)
        )


isSouthWallCord x y =
    y >= pathSize


isRightWallCord x y =
    x >= pathSize


drawInnerGridCell cellData x y =
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
