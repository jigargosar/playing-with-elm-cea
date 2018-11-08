module PopupMenu exposing (Config, Msg, State, init, isOpen, popOpen, render, subscriptions, update)

import BasicsX exposing (attemptDomIdFocus, unwrapMaybe)
import Browser.Events
import Css exposing (..)
import Debouncer exposing (Debouncer)
import DomEvents exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, styled)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed exposing (node)
import Json.Decode as D
import Log
import Port
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias State =
    { open : Bool
    , debouncer : Debouncer
    , refDomId : DomId
    , popperDomId : DomId
    , focusOnOpenDomId : Maybe DomId
    }


init : DomId -> DomId -> Maybe DomId -> State
init refDomId popperDomId focusOnOpenDomId =
    { open = False
    , debouncer = Debouncer.init
    , refDomId = refDomId
    , popperDomId = popperDomId
    , focusOnOpenDomId = focusOnOpenDomId
    }


isOpen =
    .open


popOpen =
    PopOpen


type alias BounceMsg child =
    Maybe (Msg child)


type Msg child
    = NoOp
    | Warn Log.Line
    | ChildSelected child
    | PopOpen
    | UpdateDebouncer (Debouncer.Msg (BounceMsg child))
    | DocumentFocusChanged Bool
    | PopupFocusChanged Bool
    | DebouncedCloseReceived



--    | BrowserMouseClicked


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


subscriptions state =
    Sub.batch
        [ --        Browser.Events.onClick (D.succeed BrowserMouseClicked)
          Port.documentFocusChanged DocumentFocusChanged
        ]


update : Config msg child -> Msg child -> State -> ( State, Cmd msg )
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

        setOpen bool model =
            { model | open = bool }
    in
    (case message of
        NoOp ->
            identity

        Warn logLine ->
            addCmd (Log.warn "Mode.elm" logLine)

        ChildSelected child ->
            mapModel (setOpen False)
                >> addMsg (config.selected child)

        PopOpen ->
            mapModel (setOpen True)
                >> addEffect (.focusOnOpenDomId >> unwrapMaybe Cmd.none focusDomId)
                >> addEffect (\model -> Port.createPopper ( model.refDomId, model.popperDomId ))

        DebouncedCloseReceived ->
            mapModel (setOpen False)

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
            --            if hasFocus then
            --                identity
            --
            --            else
            andThenUpdate cancelDebounceMsg

        --            andMapIf (\{ open } -> open && not hasFocus)
        --                (andThenUpdate cancelDebounceMsg)
        --
        PopupFocusChanged hasFocus ->
            andThenUpdate <|
                if hasFocus then
                    cancelDebounceMsg

                else
                    debounceCloseMsg
    )
        << pure


debouncerConfig : Debouncer.Config (Msg child) (Maybe (Msg child))
debouncerConfig =
    { toMsg = UpdateDebouncer
    , wait = 0
    , onEmit = unwrapMaybe NoOp identity
    }


type alias ViewConfig child msg =
    { toMsg : Msg child -> msg
    , state : State
    , children : List child
    , containerStyles : List Css.Style
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render { toMsg, children, containerStyles, childContent, state } =
    let
        attrToMsg =
            HA.map toMsg

        wrapAttrs =
            List.map attrToMsg

        viewChild child =
            div
                (wrapAttrs [ onClick <| ChildSelected child ])
                (childContent child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , if state.open then
                Css.batch []

              else
                Css.batch [ display none ]

            --            , position absolute
            ]
                ++ containerStyles
    in
    sDiv rootStyles
        (wrapAttrs [ id state.popperDomId, onFocusOut <| PopupFocusChanged False, onFocusIn <| PopupFocusChanged True ])
        (List.map viewChild children)
