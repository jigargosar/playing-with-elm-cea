module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , init
    , isOpenForContextId
    , popperId
    , refId
    , subscriptions
    , toggleOpenFor
    , update
    , view
    )

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import Debouncer exposing (Debouncer)
import DomEvents exposing (DomId, onFocusIn, onFocusOut)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes as HA exposing (autofocus, id)
import Html.Styled.Events exposing (onClick)
import Log
import Port
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    , debouncer : Debouncer
    , refDomId : DomId
    , popperDomId : DomId
    , focusOnOpenDomId : Maybe DomId
    , cid : ContextId
    }


init : ContextId -> Model
init cid =
    let
        popperDomId =
            popperId cid
    in
    { open = False
    , debouncer = Debouncer.init
    , refDomId = refId cid
    , popperDomId = popperDomId
    , focusOnOpenDomId = actions |> List.head |> Maybe.map (getChildDomId popperDomId)
    , cid = cid
    }


isOpenForContextId cid model =
    model.cid == cid && model.open


type alias BounceMsg =
    Maybe Msg


toggleOpenFor =
    ToggleOpenFor


type Msg
    = NoOp
    | Warn Log.Line
    | ActionClicked Action
    | ToggleOpenFor ContextId
    | UpdateDebouncer (Debouncer.Msg BounceMsg)
    | DocumentFocusChanged Bool
    | PopupFocusChanged Bool
    | DebouncedCloseReceived


subscriptions model =
    Sub.batch
        [ Port.documentFocusChanged DocumentFocusChanged
        ]


update config message =
    let
        andThenUpdate msg =
            andThen (update config msg)

        focusDomId domId =
            attemptDomIdFocus domId NoOp Warn |> Cmd.map config.toMsg

        bounce action =
            andThenUpdate (UpdateDebouncer <| Debouncer.bounce action)

        debounceCloseMsg =
            UpdateDebouncer << Debouncer.bounce <| Just DebouncedCloseReceived

        cancelDebounceMsg =
            UpdateDebouncer << Debouncer.bounce <| Nothing

        setClose model =
            { model | open = False }

        setOpenFor cid model =
            { model | open = True, cid = cid }
    in
    (case message of
        NoOp ->
            identity

        Warn logLine ->
            addCmd (Log.warn "Mode.elm" logLine)

        ActionClicked child ->
            mapModel setClose
                >> addMsg (config.selected "" child)

        ToggleOpenFor cid ->
            andMapIfElse (isOpenForContextId cid)
                (mapModel setClose)
                (mapModel (setOpenFor cid)
                    >> addEffect (.focusOnOpenDomId >> unwrapMaybe Cmd.none focusDomId)
                    >> addEffect (\model -> Port.createPopper ( model.refDomId, model.popperDomId ))
                )

        DebouncedCloseReceived ->
            mapModel setClose

        UpdateDebouncer msg ->
            andThen
                (updateSub
                    (Debouncer.update debouncerConfig)
                    .debouncer
                    (\s b -> { b | debouncer = s })
                    msg
                    >> mapCmd config.toMsg
                )

        DocumentFocusChanged hasFocus ->
            andThenUpdate cancelDebounceMsg

        PopupFocusChanged hasFocus ->
            andThenUpdate <|
                if hasFocus then
                    cancelDebounceMsg

                else
                    debounceCloseMsg
    )
        << pure


debouncerConfig : Debouncer.Config Msg (Maybe Msg)
debouncerConfig =
    { toMsg = UpdateDebouncer
    , wait = 0
    , onEmit = unwrapMaybe NoOp identity
    }


type Action
    = Rename
    | Archive


actions =
    [ Rename, Archive ]


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


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
    let
        popperDomId =
            popperId model.cid

        attrToMsg =
            HA.map toMsg

        wrapAttrs =
            List.map attrToMsg

        viewChild child =
            div
                (wrapAttrs [ onClick <| ActionClicked child ])
                (childContent popperDomId child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , pRm 0.5
            , minWidth (rem 10)
            , if model.open then
                Css.batch []

              else
                Css.batch [ display none ]

            --            , position absolute
            ]
    in
    sDiv rootStyles
        (wrapAttrs [ id model.popperDomId, onFocusOut <| PopupFocusChanged False, onFocusIn <| PopupFocusChanged True ])
        (List.map viewChild actions)
