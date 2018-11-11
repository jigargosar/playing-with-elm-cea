module BottomNav exposing (view)

import Btn
import Css exposing (..)
import Html.Styled exposing (text)
import Styles exposing (..)
import UI exposing (..)


view =
    sDiv [ flexShrink zero, rowCY, pRm 1, elevation 4, fDir rowReverse ]
        []
        [ Btn.flat [] [ text "Add" ]
        , Btn.flat [] [ text "Contexts" ]
        ]
