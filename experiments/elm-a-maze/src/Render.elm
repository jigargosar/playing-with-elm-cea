module Render exposing (..)

import Color exposing (Color)
import PairA exposing (Float2)
import Svg
import TypedSvg.Attributes.InPx as TP


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


cellSquareSize =
    1 - wallThickness


cellSquareCenterSize =
    cellSquareSize / 2


cellSqXY =
    PairA.map


defaultDia =
    cellSquareSize - (cellSquareSize / 10)


defaultR =
    defaultDia / 2


defaultCircle ( x, y ) =
    Svg.circle [ cxp x, cyp y, rp defaultR ] []


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
