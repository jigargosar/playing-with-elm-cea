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

        autoFocusOnOpenCmd model =
            Cmd.map tagger <|
                Focus.attemptMaybe FocusResult (getMaybeAutoFocusDomId model)
    in
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)

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
    )
        << pure


bounceMaybeMsg { tagger, emitIfCountMsg } maybeMsg model =
    let
        bounceCount =
            model.bounceCount + 1
    in
    ( { model | bounceCount = bounceCount }
    , afterTimeout 0 (emitIfCountMsg bounceCount maybeMsg)
    )
        |> mapCmd tagger


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
            ]
    in
    sDiv rootStyles
        (wrapAttrs rootAttributes)
        (List.map viewChild actions)
