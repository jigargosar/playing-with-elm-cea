module Btn exposing (flat, flatPl0, flatPr0, icon, sIcon)

import Css exposing (hover)
import Css.Transitions as CT exposing (transition)
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Icons
import Styles exposing (..)


hoverTransition =
    Css.batch
        [ hover [ fg "red" ]
        , transition [ CT.color3 250 0 CT.easeIn ]
        ]


outlineTransition =
    Css.batch
        [ transition
            [ CT.outlineOffset3 150 0 CT.easeIn
            , CT.outlineWidth3 150 0 CT.easeIn

            --            , CT.outline3 150 0 CT.easeIn
            ]
        ]


icon =
    styled button
        [ btnReset
        , fZero
        , hoverTransition
        , outlineTransition
        ]


sIcon =
    styled icon


flat =
    styled button
        [ btnReset
        , p2Rm 0 0.5
        , fgGray
        , rowCY
        , hoverTransition
        , outlineTransition
        , Css.focus [ outlineTransition ]
        ]


flatPl0 =
    styled flat [ plRm 0 ]


flatPr0 =
    styled flat [ prRm 0 ]



--flat =
--    styled button
--        [ btnReset
--        , pRm 0.5
--
--        --        , flexAuto
--        , Css.fontSize (rem 0.8)
--        , Css.property "color" "gray"
--        , Css.hover
--            [ Css.property "color" "red"
--            ]
--        ]
