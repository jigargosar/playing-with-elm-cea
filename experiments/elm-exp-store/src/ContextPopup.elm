module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , init
    , isOpenForContextId
    , popperId
    , refId
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
    let
        popperDomId =
            popperId cid
    in
    { popupState =
        PopupMenu.init
            (refId cid)
            popperDomId
            (actions |> List.head |> Maybe.map (getChildDomId popperDomId))
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
    | Archive


actions =
    [ Rename, Archive ]


type alias Msg =
    PopupMenu.Msg Action


popperId cid =
    "context-more-menu-popper-" ++ cid


refId cid =
    "context-more-menu-reference-" ++ cid


getChildText child =
    case child of
        Rename ->
            "Rename"

        Archive ->
            "Archive"


getChildDomId popperDomId child =
    popperDomId ++ "-" ++ getChildText child


childContent popperDomId child =
    [ sDiv [ p2Rm 0 0 ]
        []
        [ styled button
            [ btnReset, p2Rm 0.5 1, w100 ]
            [ id <| getChildDomId popperDomId child
            ]
            [ text <| getChildText child
            ]
        ]
    ]


type alias ViewConfig msg =
    { toMsg : Msg -> msg, state : Model }


view : ViewConfig msg -> Html msg
view { toMsg, state } =
    let
        popperDomId =
            popperId state.cid
    in
    PopupMenu.render
        { toMsg = toMsg
        , state = state.popupState
        , children = actions
        , containerStyles =
            [ pRm 0.5
            , minWidth (rem 10)
            ]
        , childContent = childContent popperDomId
        }
