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
    , bounceMsg : Maybe Msg
    , debouncer : Debouncer
    , cid : ContextId
    , popperStyles : PopperStyles
    }


init : ContextId -> Model
init cid =
    { open = False
    , bounceCount = 0
    , bounceMsg = Nothing
    , debouncer = Debouncer.init
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
      --    | UpdateDebouncer (Debouncer.Msg BounceMsg)
    | DocumentFocusChanged Bool
    | PopupFocusChanged Bool
    | DebouncedClose
    | EmitIfCountEq Int (Maybe Msg)
    | PopperStylesChanged PopperStyles


subscriptions model =
    Sub.batch
        [ Port.documentFocusChanged DocumentFocusChanged
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
        cancelBounceMsg =
            bounceMaybeMsg Nothing

        bounceCloseMsg =
            bounceMaybeMsg <| Just DebouncedClose

        bounceMaybeMsg maybeMsg =
            andThen
                (\model ->
                    let
                        bounceCount =
                            model.bounceCount + 1
                    in
                    ( { model | bounceCount = bounceCount }
                    , afterTimeout 0 (EmitIfCountEq bounceCount maybeMsg)
                    )
                        |> mapCmd config.toMsg
                )

        andThenUpdate msg =
            andThen (update config msg)

        focusDomId domId =
            attemptDomIdFocus domId NoOp Warn |> Cmd.map config.toMsg

        setOpenFor cid model =
            { model | open = True, cid = cid }

        createPopperCmd { cid } =
            Port.createPopper ( refId cid, popperId cid )

        closeAndDestroyPopper =
            andMapWhen .open
                (mapModel (\model -> { model | open = False })
                    >> addCmd (Port.destroyPopper ())
                )

        incCount model =
            { model | bounceCount = model.bounceCount + 1 }
    in
    (case message of
        NoOp ->
            identity

        Warn logLine ->
            addCmd (Log.warn "Mode.elm" logLine)

        ActionClicked child ->
            let
                actionSelectedCmd : Model -> Cmd msg
                actionSelectedCmd model =
                    Task.perform identity (Task.succeed <| config.selected model.cid child)
            in
            closeAndDestroyPopper
                >> addEffect actionSelectedCmd

        ToggleOpenFor cid ->
            andMapIfElse (isOpenForContextId cid)
                closeAndDestroyPopper
                (mapModel (setOpenFor cid)
                    >> addEffect (getAutoFocusDomId >> unwrapMaybe Cmd.none focusDomId)
                    >> addEffect createPopperCmd
                )

        EmitIfCountEq count msg ->
            andMapWhen (.bounceCount >> eqs count)
                (unwrapMaybe identity andThenUpdate msg)

        DebouncedClose ->
            closeAndDestroyPopper

        DocumentFocusChanged hasFocus ->
            cancelBounceMsg

        PopupFocusChanged hasFocus ->
            if hasFocus then
                cancelBounceMsg

            else
                bounceCloseMsg

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
                Css.batch [ opacity (int 0), transitionFadeIn ]
            , left (rem -100)
            , position absolute
            ]

        rootAttributes =
            [ id popperDomId
            , onFocusOut <| PopupFocusChanged False
            , onFocusIn <| PopupFocusChanged True
            ]
                ++ List.map (\( n, v ) -> attribute n v) model.popperStyles.attributes
                ++ List.map (\( n, v ) -> style n v) model.popperStyles.styles
    in
    sDiv rootStyles
        (wrapAttrs rootAttributes)
        (List.map viewChild actions)
