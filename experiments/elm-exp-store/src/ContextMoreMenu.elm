module ContextMoreMenu exposing (Action(..), actions, childContent, contextMoreMenuPopperDomId, contextMoreMenuRefDomId, view)

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import Html.Styled exposing (button, styled, text)
import PopupMenu
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


contextMoreMenuPopperDomId =
    "context-more-menu-popper"


contextMoreMenuRefDomId cid =
    "context-more-menu-reference-" ++ cid


view config state =
    PopupMenu.render
        { config = config
        , state = state
        , domId = contextMoreMenuPopperDomId
        , children = actions
        , containerStyles =
            [ pRm 0.5
            , minWidth (rem 20)
            ]
        , childContent = childContent
        }
