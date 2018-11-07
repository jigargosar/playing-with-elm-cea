module PopupMenu exposing (Config, Msg, State, closed, opened, render, update)

import Css exposing (..)
import DomEvents exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, styled)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed exposing (node)
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias State =
    { open : Bool
    }


opened =
    State True


closed =
    State True


type Msg child
    = NoOp
    | ChildSelected child


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


update : Config msg child -> Msg child -> State -> ( State, Cmd msg )
update config message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ChildSelected child ->
            pure { model | open = False }
                |> addMsg (config.selected child)


type alias ViewConfig child msg =
    { config : Config msg child
    , state : State
    , domId : DomId
    , children : List child
    , containerStyles : List Css.Style
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render { config, children, containerStyles, domId, childContent, state } =
    let
        attrToMsg =
            HA.map config.toMsg

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
