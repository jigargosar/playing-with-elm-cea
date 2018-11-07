module ContextMoreMenu exposing (Action(..), actions, childContent)

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import CssAtoms exposing (..)
import Html.Styled exposing (button, styled, text)
import Styles exposing (..)
import UI exposing (..)


type Action
    = Rename
    | Delete


actions =
    [ Rename, Delete ]


childContent child =
    [ sDiv [ p2Rm 0 0 ]
        []
        [ styled button
            [ btnReset, p2Rm 0.5 1, w100 ]
            []
            [ text
                (case child of
                    Rename ->
                        "Rename"

                    Delete ->
                        "Delete"
                )
            ]
        ]
    ]
