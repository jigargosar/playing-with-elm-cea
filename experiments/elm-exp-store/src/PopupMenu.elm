module PopupMenu exposing (Config, Msg, State, init, popOpen, render, update)

import Css exposing (..)
import DomEvents exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, styled)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed exposing (node)
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


type Msg child
    = ChildSelected child
    | PopOpen


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


update : Config msg child -> Msg child -> State -> ( State, Cmd msg )
update config message model =
    case message of
        ChildSelected child ->
            pure { model | open = False }
                --                |> Port.destroyPopper
                |> addMsg (config.selected child)

        PopOpen ->
            pure { model | open = True }
                |> addCmd (Port.createPopper ( model.refDomId, model.popperDomId ))


type alias ViewConfig child msg =
    { toMsg : Msg child -> msg
    , state : State
    , domId : DomId
    , children : List child
    , containerStyles : List Css.Style
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render { toMsg, children, containerStyles, domId, childContent, state } =
    let
        attrToMsg =
            HA.map toMsg

        viewChild child =
            div [ (onClick <| ChildSelected child) |> attrToMsg ] (childContent child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , position absolute
            ]
                ++ containerStyles
    in
    if state.open then
        sDiv rootStyles
            [ id domId ]
            (children |> List.map viewChild)

    else
        noHtml
