module PopupMenu exposing (Config, Msg, State, init, isOpen, popOpen, render, subscriptions, update)

import BasicsX exposing (attemptDomIdFocus)
import Browser.Events
import Css exposing (..)
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
    , refDomId : DomId
    , popperDomId : DomId
    }


init : DomId -> DomId -> State
init refDomId popperDomId =
    { open = False, refDomId = refDomId, popperDomId = popperDomId }


popOpen =
    PopOpen


isOpen =
    .open


type Msg child
    = NoOp
    | Warn Log.Line
    | FocusDomId DomId
    | ChildSelected child
    | PopOpen
    | BrowserMouseClicked


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


subscriptions state =
    Sub.batch
        [ Browser.Events.onClick (D.succeed BrowserMouseClicked)
        ]


update : Config msg child -> Msg child -> State -> ( State, Cmd msg )
update config message model =
    case message of
        NoOp ->
            pure model

        Warn logLine ->
            pure model
                |> addCmd (Log.warn "Mode.elm" logLine)

        FocusDomId domId ->
            pure model
                |> addMapCmd config.toMsg (attemptDomIdFocus domId NoOp Warn)

        ChildSelected child ->
            pure { model | open = False }
                --                |> Port.destroyPopper
                |> addMsg (config.selected child)

        PopOpen ->
            pure { model | open = True }
                |> addCmd (Port.createPopper ( model.refDomId, model.popperDomId ))

        BrowserMouseClicked ->
            --            pure { model | open = False }
            pure model



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
        [ id state.popperDomId ]
        (children |> List.map viewChild)



--    if state.open then
--        sDiv rootStyles
--            [ id state.popperDomId ]
--            (children |> List.map viewChild)
--
--    else
--        noHtml
--
