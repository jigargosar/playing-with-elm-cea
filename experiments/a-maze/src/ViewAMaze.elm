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


cellSize =
    pathSize + wallSize


innerCellSizeInPx =
    10


cellSizeInPx =
    innerCellSizeInPx * cellSize


viewMaze maze =
    g [] (AMaze.mapData drawMazeCellAt maze)


drawMazeCellAt cellX cellY cellData =
    g
        [ iTranslate
            (cellX * cellSizeInPx)
            (cellY * cellSizeInPx)
            |> SA.transform
        ]
        (mapCoordinates2D
            cellSize
            cellSize
            (drawInnerCellWithDataAt cellData)
        )


isSouthWallCord x y =
    y >= pathSize


isRightWallCord x y =
    x >= pathSize


drawInnerCellWithDataAt cellData x y =
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


drawInnerCell x y fillS =
    rect
        [ x * innerCellSizeInPx |> iX
        , y * innerCellSizeInPx |> iY
        , iWidth innerCellSizeInPx
        , iHeight innerCellSizeInPx
        , SA.fill fillS
        ]
        []
