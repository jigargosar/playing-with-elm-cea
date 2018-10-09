module Render exposing (..)

import PairA
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


circle ( x, y ) =
    Svg.circle [ cxp x, cyp y ] []
