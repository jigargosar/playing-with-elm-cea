module AppBar exposing (Config, view)

import Btn
import Css exposing (fontWeight, lighter)
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
        [ UI.section1 [ class "pa3" ]
            [ Btn.icon [] [ Icons.plusDef ]
            , sDiv [ fwb ] [] [ text "ELM" ]
            , sDiv [ fontWeight lighter ] [] [ text "DONE" ]
            ]
        ]
