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
import Bouncer
import Browser.Dom
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import DomX exposing (DomId, onFocusIn, onFocusOut)
import Focus exposing (FocusResult)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes as HA exposing (attribute, autofocus, id, style)
import Html.Styled.Events exposing (onClick)
import Log
import Styles exposing (..)
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    , bounceCount : Int
    , cid : ContextId
    }


init : ContextId -> Model
init cid =
    { open = False
    , bounceCount = 0
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
    | ActionClicked Action
    | ToggleOpenFor ContextId
    | PopupFocusChanged Bool
    | DebouncedClose ContextId
    | EmitIfBounceCount Int (Maybe Msg)
    | FocusResult FocusResult


subscriptions model =
    Sub.batch
        []


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : ContextId -> Action -> msg
    }


getPopperDomId =
    .cid >> popperId


getMaybeAutoFocusDomId model =
    actions |> List.head |> Maybe.map (getChildDomId (getPopperDomId model))


setClosed =
    \model -> { model | open = False }


setOpenAndContextId cid model =
    { model | open = True, cid = cid }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message =
    let
        tagger =
            config.toMsg

        bouncerConfig =
            { tagger = tagger, emitIfCountMsg = EmitIfBounceCount }

        autoFocusOnOpenCmd model =
            Cmd.map tagger <|
                Focus.attemptMaybe FocusResult (getMaybeAutoFocusDomId model)
    in
    (case message of
        NoOp ->
            identity

        FocusResult r ->
            addCmd (Log.focusResult "Mode.elm" r)

        ActionClicked child ->
            mapModel setClosed
                >> addMsgEffect (.cid >> (\cid -> config.selected cid child))

        ToggleOpenFor cid ->
            andThen
                (\model ->
                    if isOpenForContextId cid model then
                        let
                            newModel =
                                setClosed model
                        in
                        ( newModel, Cmd.none )

                    else
                        let
                            newModel =
                                setOpenAndContextId cid model
                        in
                        ( newModel, autoFocusOnOpenCmd newModel )
                )

        EmitIfBounceCount count maybeMsg ->
            Bouncer.emitIfBounceCount bouncerConfig count maybeMsg

        DebouncedClose cid ->
            andMapWhen (.cid >> eqs cid) (mapModel setClosed)

        PopupFocusChanged hasFocus ->
            andThen <|
                if hasFocus then
                    Bouncer.cancel bouncerConfig

                else
                    \model -> Bouncer.bounce bouncerConfig (DebouncedClose model.cid) model
    )
        << pure


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
            , position absolute
            , bottom (px 0)
            ]

        rootAttributes =
            [ id popperDomId
            , onFocusOut <| PopupFocusChanged False
            , onFocusIn <| PopupFocusChanged True
            ]
    in
    sDiv rootStyles
        (wrapAttrs rootAttributes)
        (List.map viewChild actions)
