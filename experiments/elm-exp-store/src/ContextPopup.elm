module ContextPopup exposing (Action(..), Msg, contextMoreMenuPopperDomId, contextMoreMenuRefDomId, view)

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import Html.Styled exposing (Html, button, styled, text)
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


contextMoreMenuPopperDomId cid =
    "context-more-menu-popper-" ++ cid


contextMoreMenuRefDomId cid =
    "context-more-menu-reference-" ++ cid


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


type alias ViewConfig msg =
    { toMsg : PopupMenu.Msg Action -> msg, cid : ContextId, state : PopupMenu.State }


view : ViewConfig msg -> Html msg
view { cid, toMsg, state } =
    PopupMenu.render
        { toMsg = toMsg
        , state = state
        , children = actions
        , containerStyles =
            [ pRm 0.5
            , minWidth (rem 10)
            ]
        , childContent = childContent
        }
