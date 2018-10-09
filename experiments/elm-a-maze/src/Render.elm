module Render exposing (..)

import Basics.Extra exposing (flip)
import Color exposing (Color)
import Maze
import PairA exposing (Float2)
import Ramda exposing (ter)
import Svg
import Svg as S
import Svg.Attributes as SA
import Svg.Attributes as S
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


defaultCircle ( x, y ) color =
    Svg.circle [ cxp x, cyp y, rp defaultR, color |> Color.toCssString >> S.fill ] []


cellCircle xy =
    defaultCircle (PairA.add (1 / 2) xy)


cellCircleWithColor =
    flip cellCircle


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
    cellCircleWithColor Color.darkOrange


viewPlayerXY =
    cellCircleWithColor Color.white


viewPortalXY =
    cellCircleWithColor Color.darkPurple


viewKeyXY =
    cellCircleWithColor Color.darkGray
