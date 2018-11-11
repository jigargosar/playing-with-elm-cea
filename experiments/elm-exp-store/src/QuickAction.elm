module QuickAction exposing (view)

import Btn
import Css exposing (..)
import Html.Styled exposing (text)
import Styles exposing (..)
import UI exposing (..)


view =
    sDiv [ flexShrink zero, rowCXY, pRm 1, elevation 4 ]
        []
        [ Btn.flat [] [ text "Add Task" ]
        , Btn.flat [] [ text "Add Context" ]
        , Btn.flat [] [ text "View Contexts" ]
        , Btn.flat [] [ text "View Completed" ]
        , Btn.flat [] [ text "View Archived Contexts" ]
        ]
