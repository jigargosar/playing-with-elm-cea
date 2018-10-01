module ISvg exposing (iCX, iCY, iHeight, iScale, iTranslate, iWidth, iX, iY)

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


iScale x y =
    [ "scale("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ")"
    ]
        |> String.join ""


iX =
    String.fromInt >> SA.x


iCX =
    String.fromInt >> SA.cx


iY =
    String.fromInt >> SA.y


iCY =
    String.fromInt >> SA.cy


iWidth =
    String.fromInt >> SA.width


iHeight =
    String.fromInt >> SA.height
