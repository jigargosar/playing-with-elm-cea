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
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import DomEvents exposing (DomId, onFocusIn, onFocusOut)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes as HA exposing (attribute, autofocus, id, style)
import Html.Styled.Events exposing (onClick)
import Log
import Port exposing (PopperStyles)
import Styles exposing (..)
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    , bounceCount : Int
    , cid : ContextId
    , popperStyles : PopperStyles
    }


init : ContextId -> Model
init cid =
    { open = False
    , bounceCount = 0
    , cid = cid
    , popperStyles = { styles = [], attributes = [] }
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
    | DocumentFocusChanged Bool
    | PopupFocusChanged Bool
    | DebouncedClose
    | EmitIfBounceCount Int (Maybe Msg)
    | PopperStylesChanged PopperStyles
    | PopperStylesSet PopperStyles


subscriptions model =
    Sub.batch
        [ Port.documentFocusChanged DocumentFocusChanged
        , Port.popperStylesSet PopperStylesSet
        , Port.popperStylesChanged PopperStylesChanged
        ]


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : ContextId -> Action -> msg
    }


getPopperDomId =
    .cid >> popperId


getAutoFocusDomId model =
    actions |> List.head |> Maybe.map (getChildDomId (getPopperDomId model))


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message =
    let
        tagger =
            config.toMsg

        bouncerConfig =
            { tagger = tagger, emitIfCountMsg = EmitIfBounceCount }

        setOpenFor cid model =
            { model | open = True, cid = cid }

        attachPopperCmd { cid } =
            Port.createPopper ( refId cid, popperId cid )

        closeAndDestroyPopper =
            andMapWhen .open
                (mapModel (\model -> { model | open = False })
                    >> addEffect (\{ cid } -> Port.destroyPopper ( refId cid, popperId cid ))
                )

        autoFocus =
            addTaggedEffect tagger (getAutoFocusDomId >> attemptFocusMaybeDomId NoOp Warn)
    in
    (case message of
        NoOp ->
            identity

        Warn logLine ->
            addCmd (Log.warn "Mode.elm" logLine)

        ActionClicked child ->
            closeAndDestroyPopper
                >> addMsgEffect (.cid >> (\cid -> config.selected cid child))

        ToggleOpenFor cid ->
            andMapIfElse (isOpenForContextId cid)
                closeAndDestroyPopper
                (closeAndDestroyPopper
                    >> mapModel (setOpenFor cid)
                    >> addEffect attachPopperCmd
                )

        EmitIfBounceCount count maybeMsg ->
            Bouncer.emitIfBounceCount bouncerConfig count maybeMsg

        DebouncedClose ->
            closeAndDestroyPopper

        DocumentFocusChanged hasFocus ->
            andThen (Bouncer.cancel bouncerConfig)

        PopupFocusChanged hasFocus ->
            if hasFocus then
                andThen (Bouncer.cancel bouncerConfig)

            else
                andThen (Bouncer.bounce bouncerConfig DebouncedClose)

        PopperStylesSet popperStyles ->
            let
                _ =
                    Debug.log "PopperStylesSet" popperStyles
            in
            mapModel (\model -> { model | popperStyles = popperStyles })
                >> autoFocus

        PopperStylesChanged popperStyles ->
            mapModel (\model -> { model | popperStyles = popperStyles })
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
                Css.batch [ opacity (int 1), transitionFadeIn ]

              else
                Css.batch
                    [ opacity (int 0)
                    , transitionFadeIn
                    , left (rem -100)
                    ]
            , position absolute
            ]

        rootAttributes =
            [ id popperDomId
            , onFocusOut <| PopupFocusChanged False
            , onFocusIn <| PopupFocusChanged True
            ]
                ++ (if model.open then
                        List.map (\( n, v ) -> attribute n v) model.popperStyles.attributes
                            ++ List.map (\( n, v ) -> style n v) model.popperStyles.styles

                    else
                        []
                   )
    in
    sDiv rootStyles
        (wrapAttrs rootAttributes)
        (List.map viewChild actions)
