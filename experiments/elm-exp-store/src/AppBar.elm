module AppBar exposing (Config, view)

import Btn
import Css exposing (fontSize, fontWeight, lighter, rem)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Icons
import Styles exposing (..)
import UI exposing (..)


type alias Config msg =
    { menuClicked : msg
    }


view =
    UI.appBar []
        [ styled UI.section1
            [ fzPx 24 ]
            [ class "pa3" ]
            [ Btn.sIcon [ hs ] [] [ Icons.plusDef ]
            , sDiv [ fwb ] [] [ text "ELM" ]
            , sDiv [ fontWeight lighter ] [] [ text "DONE" ]
            ]
        ]
