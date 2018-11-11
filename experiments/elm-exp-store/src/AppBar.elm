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


view : Config msg -> Html msg
view config =
    let
        { menuClicked } =
            config
    in
    UI.appBar []
        [ styled UI.section1
            [ pRm 1, rowCY ]
            []
            [ Btn.sIcon [ hs ] [ class "dn-ns" ] [ Icons.menuDef ]
            , sDiv [ fzPx 24, rowCY, hs ]
                []
                [ sDiv [ fwb ] [] [ text "ELM" ]
                , sDiv [ fwl ] [] [ text "DONE" ]
                ]
            ]
        ]
