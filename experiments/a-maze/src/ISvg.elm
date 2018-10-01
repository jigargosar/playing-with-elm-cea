module ISvg exposing
    ( iCX
    , iCY
    , iFontSize
    , iHeight
    , iScale
    , iTranslate
    , iWidth
    , iX
    , iX1
    , iX2
    , iY
    , iY1
    , iY2
    )

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


iX1 =
    String.fromInt >> SA.x1


iX2 =
    String.fromInt >> SA.x2


iCX =
    String.fromInt >> SA.cx


iY =
    String.fromInt >> SA.y


iY1 =
    String.fromInt >> SA.y1


iY2 =
    String.fromInt >> SA.y2


iCY =
    String.fromInt >> SA.cy


iWidth =
    String.fromInt >> SA.width


iHeight =
    String.fromInt >> SA.height


iFontSize =
    String.fromInt >> SA.fontSize
