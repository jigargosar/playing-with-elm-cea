module ContextPopup exposing (Action(..), Msg, actions, childContent, contextMoreMenuPopperDomId, contextMoreMenuRefDomId, view)

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


type alias Msg =
    PopupMenu.Msg Action


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


contextMoreMenuPopperDomId cid =
    "context-more-menu-popper-"


contextMoreMenuRefDomId cid =
    "context-more-menu-reference-" ++ cid


view config cid state =
    PopupMenu.render
        { config = config
        , state = state
        , domId = contextMoreMenuPopperDomId cid
        , children = actions
        , containerStyles =
            [ pRm 0.5
            , minWidth (rem 10)
            ]
        , childContent = childContent
        }
