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


popOpen =
    PopOpen


isOpen =
    .open


type Msg child
    = NoOp
    | Warn Log.Line
    | ChildSelected child
    | PopOpen
    | UpdateDebouncer (Debouncer.Msg Bool)
    | FocusOut
    | Bounced Bool



--    | BrowserMouseClicked


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


subscriptions state =
    Sub.batch
        [--        Browser.Events.onClick (D.succeed BrowserMouseClicked)
        ]


update : Config msg child -> Msg child -> State -> ( State, Cmd msg )
update config message =
    let
        focusDomId domId =
            attemptDomIdFocus domId NoOp Warn |> Cmd.map config.toMsg
    in
    (case message of
        NoOp ->
            identity

        Warn logLine ->
            addCmd (Log.warn "Mode.elm" logLine)

        ChildSelected child ->
            mapModel (\model -> { model | open = False })
                >> addMsg (config.selected child)

        PopOpen ->
            mapModel (\model -> { model | open = True })
                >> addEffect (.focusOnOpenDomId >> unwrapMaybe Cmd.none focusDomId)
                >> addEffect (\model -> Port.createPopper ( model.refDomId, model.popperDomId ))

        Bounced msg ->
            identity

        UpdateDebouncer msg ->
            let
                dConfig =
                    { toMsg = UpdateDebouncer, wait = 0, onEmit = Bounced }
            in
            andThen
                (updateSub
                    (Debouncer.update dConfig)
                    .debouncer
                    (\s b -> { b | debouncer = s })
                    msg
                    >> mapCmd config.toMsg
                )

        FocusOut ->
            identity
    )
        << pure



--        BrowserMouseClicked ->
--            pure { model | open = False }
--            pure model
--                |> Port.destroyPopper


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

        viewChild child =
            div [ (onClick <| ChildSelected child) |> attrToMsg ] (childContent child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)

            --            , position absolute
            , boolCss (not state.open) [ display none ]
            ]
                ++ containerStyles
    in
    sDiv rootStyles
        ([ id state.popperDomId, onFocusOut FocusOut ] |> List.map attrToMsg)
        (children |> List.map viewChild)



--    if state.open then
--        sDiv rootStyles
--            [ id state.popperDomId ]
--            (children |> List.map viewChild)
--
--    else
--        noHtml
--
