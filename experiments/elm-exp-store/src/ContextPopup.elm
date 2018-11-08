module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , contextMoreMenuPopperDomId
    , contextMoreMenuRefDomId
    , init
    , isOpenForContextId
    , subscriptions
    , update
    , view
    )

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import DomEvents exposing (DomId)
import Html.Styled exposing (Html, button, styled, text)
import Html.Styled.Attributes exposing (autofocus, id)
import PopupMenu
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (mapModel)


type alias Model =
    { popupState : PopupMenu.State
    , cid : ContextId
    }


init : ContextId -> Model
init cid =
    { popupState = PopupMenu.init (contextMoreMenuRefDomId cid) (contextMoreMenuPopperDomId cid)
    , cid = cid
    }


isOpenForContextId cid model =
    model.cid == cid && PopupMenu.isOpen model.popupState


subscriptions model =
    PopupMenu.subscriptions model.popupState


update { toMsg, selected } msg model =
    PopupMenu.update { toMsg = toMsg, selected = selected model.cid } msg model.popupState
        |> mapModel (\s -> { model | popupState = s })


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



--type alias ChildViewModel =
--   {name: String, domId: DomId}
--
--createChildViewModel child =
--    case child of
--        Rename ->
--                 ChildViewModel  "Rename"
--
--        Delete ->
--                                "Delete"
--


childContent cid child =
    [ sDiv [ p2Rm 0 0 ]
        []
        [ styled button
            [ btnReset, p2Rm 0.5 1, w100 ]
            [ id <|
                case child of
                    Rename ->
                        contextMoreMenuPopperDomId cid ++ "-" ++ "autofocus"

                    --                        "Rename"
                    Delete ->
                        "Delete"
            ]
            [ text <|
                case child of
                    Rename ->
                        "Rename"

                    Delete ->
                        "Delete"
            ]
        ]
    ]


type alias ViewConfig msg =
    { toMsg : Msg -> msg, state : Model }


view : ViewConfig msg -> Html msg
view { toMsg, state } =
    PopupMenu.render
        { toMsg = toMsg
        , state = state.popupState
        , children = actions
        , containerStyles =
            [ pRm 0.5
            , minWidth (rem 10)
            ]
        , childContent = childContent state.cid
        }
