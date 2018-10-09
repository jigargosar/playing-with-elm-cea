module Render exposing (..)

import Basics.Extra exposing (curry, flip, uncurry)
import Color exposing (Color)
import Maze
import PairA exposing (Float2)
import Ramda exposing (F, ter)
import Svg exposing (Svg)
import Svg as S
import Svg.Attributes as SA
import Svg.Attributes as S
import Svg.Lazy
import TypedSvg.Attributes.InPx as TP
import TypedSvg.Attributes as T


xp =
    TP.x


yp =
    TP.y


cxp =
    TP.cx


cyp =
    TP.cy


rp =
    TP.r


wp =
    TP.width


hp =
    TP.height


wallThickness =
    1 / 10


defaultR =
    0.5 - (wallThickness)


defaultCircle color ( x, y ) =
    Svg.circle [ cxp x, cyp y, rp defaultR, color |> Color.toCssString >> S.fill ] []


cellCircle : Color -> Float2 -> Svg msg
cellCircle color =
    PairA.add (1 / 2) >> defaultCircle color


type Radius
    = Radius Float


type Size
    = Size Float


type Position
    = XY Float2


type Shape
    = Circle Radius
    | Square Size


type Fill
    = Fill Color
    | NoFill


type Stroke
    = Stroke Float Color
    | NoStroke


type Style
    = Style Fill Stroke


defaultStyle =
    Style (Fill Color.black) (NoStroke)


viewWall maze cord =
    let
        cellSize =
            1

        ( x, y ) =
            cord |> PairA.toFloat |> PairA.mul cellSize

        eastWall =
            Svg.rect
                [ TP.x (x + cellSize - (wallThickness / 2))
                , TP.y y
                , TP.width wallThickness
                , TP.height cellSize
                , SA.fill "#000"
                ]
                []

        southWall =
            Svg.rect
                [ TP.x x
                , TP.y (y + cellSize - (wallThickness / 2))
                , TP.width cellSize
                , TP.height wallThickness
                , SA.fill "#000"
                ]
                []
    in
        ter (Maze.isEastConnected cord maze) [] [ eastWall ]
            ++ ter (Maze.isSouthConnected cord maze) [] [ southWall ]


viewMonsterXY =
    lazyCellCircle Color.darkOrange


viewPlayerXY =
    lazyCellCircle Color.white


viewPortalXY =
    lazyCellCircle Color.darkPurple


viewPortalKeyXY =
    lazyCellCircle Color.darkGray


lazyCellCircle : Color -> Float2 -> Svg msg
lazyCellCircle color =
    svgLazyT (cellCircle color)


svgLazyT : F (( a, b ) -> Svg msg)
svgLazyT fnT =
    uncurry (Svg.Lazy.lazy2 (curry fnT))
