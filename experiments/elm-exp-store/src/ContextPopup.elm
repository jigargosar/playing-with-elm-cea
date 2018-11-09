module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , init
    , isOpenForContextId
    , subscriptions
    , toggleOpenFor
    , update
    , view
    )

import BasicsX exposing (..)
import Browser.Dom
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import DomX exposing (DomId, onClickTargetId, onFocusIn, onFocusOut)
import Focus exposing (FocusResult)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes as HA exposing (attribute, autofocus, id, style)
import Html.Styled.Events exposing (onClick)
import HtmlX
import Log
import Styles exposing (..)
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    , cid : ContextId
    }


init : ContextId -> Model
init cid =
    { open = False
    , cid = cid
    }


isOpenForContextId cid model =
    model.cid == cid && model.open


type alias BounceMsg =
    Maybe Msg


toggleOpenFor =
    ToggleOpenFor


type Msg
    = ActionClicked Action
    | ToggleOpenFor ContextId
    | FocusResult FocusResult
    | BackdropClicked DomId


subscriptions model =
    Sub.batch
        []


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : Action -> ContextId -> msg
    }


getPopperDomId =
    .cid >> (++) "context-popup-"


getBackdropDomId =
    getPopperDomId >> (++) "-backdrop"


getMaybeAutoFocusDomId model =
    actions |> List.head |> Maybe.map (getChildDomId (getPopperDomId model))


setClosed =
    \model -> { model | open = False }


setOpenAndContextId cid model =
    { model | open = True, cid = cid }


update : (Action -> ContextId -> msg) -> Msg -> Model -> ( Model, Cmd Msg, Maybe msg )
update onAction message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        BackdropClicked targetId ->
            mapWhen (getBackdropDomId >> eqs targetId)
                (mapModel setClosed)
                >> withNoOutMsg

        ActionClicked child ->
            mapModel setClosed
                >> withOutMsgEffect (.cid >> onAction child)

        ToggleOpenFor cid ->
            mapIfElse (isOpenForContextId cid)
                (mapModel setClosed)
                (mapModel (setOpenAndContextId cid)
                    >> addEffect (getMaybeAutoFocusDomId >> Focus.attemptMaybe FocusResult)
                )
                >> withNoOutMsg
    )
        << pure


type Action
    = Rename
    | Archive


actions =
    [ Rename, Archive ]


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


view : Model -> Html Msg
view =
    HtmlX.when .open viewPopup


viewPopup : Model -> Html Msg
viewPopup model =
    let
        popperDomId =
            getPopperDomId model

        viewChild child =
            div
                [ onClick <| ActionClicked child ]
                (childContent popperDomId child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , pRm 0.5
            , minWidth (rem 10)
            ]

        rootAttributes =
            [ id popperDomId
            ]

        viewModalContent =
            sDiv rootStyles
                rootAttributes
                (List.map viewChild actions)

        backdropAttrs =
            [ id <| getBackdropDomId model, onClickTargetId BackdropClicked ]
    in
    UI.backdrop backdropAttrs [ viewModalContent ]
