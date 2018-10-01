module ViewAMaze exposing (view)

import AMaze exposing (AMaze, MazeCellData)
import Coordinate2D exposing (Coordinate2D)
import Frame2d
import Html.Lazy
import ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)
import Point2d
import Ramda exposing (ter)
import Rectangle2d
import Round
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes as SA


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


view : AMaze -> Svg msg
view maze =
    g [] (AMaze.mapData drawMazeCellAt maze)


drawMazeCellAt : Coordinate2D -> MazeCellData -> Svg msg
drawMazeCellAt ( cellX, cellY ) cellData =
    g
        [ iTranslate
            (cellX * cellSizeInPx)
            (cellY * cellSizeInPx)
            |> SA.transform
        ]
        (Coordinate2D.flatMap
            cellSize
            cellSize
            (drawInnerCellWithDataAt cellData)
        )


isSouthWallCord x y =
    y >= pathSize


isRightWallCord x y =
    x >= pathSize


drawInnerCellWithDataAt : MazeCellData -> Coordinate2D -> Svg msg
drawInnerCellWithDataAt { down, right } ( x, y ) =
    let
        drawPath =
            drawInnerCell x y "#cd37a9"

        drawWall =
            drawInnerCell x y "#000"

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
