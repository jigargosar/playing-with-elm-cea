module ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)

import Svg.Attributes as SA



---- SVG INT HELPERS ----


iTranslate x y =
    [ "translate("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ")"
    ]
        |> String.join ""


iX =
    String.fromInt >> SA.x


iY =
    String.fromInt >> SA.y


iWidth =
    String.fromInt >> SA.width


iHeight =
    String.fromInt >> SA.height
