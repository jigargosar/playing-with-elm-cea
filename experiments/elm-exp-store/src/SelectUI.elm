module SelectUI exposing
    ( Config
    , Model
    , Msg
    , new
    , subscriptions
    , update
    , view
    )

import BasicsX exposing (..)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Port
import Process
import Task
import UI exposing (..)
import Update2
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    }


new =
    { open = False }


type Msg item
    = NoOp
    | ActiveElementParentIds (List DomId)
    | SelectClicked
    | ItemClicked item
    | OnFocusOut
    | Close
    | SetOpen Bool


type alias Config msg item =
    { onSelect : item -> msg
    , toMsg : Msg item -> msg
    , toLabel : item -> String
    , domId : DomId
    }


subscriptions : Config msg item -> Model -> Sub msg
subscriptions config model =
    Sub.batch
        [ if model.open then
            Port.activeElementsParentIdList ActiveElementParentIds |> Sub.map config.toMsg

          else
            Sub.none
        ]


setOpen : Bool -> Model -> Model
setOpen open model =
    { model | open = open }


update : Config msg item -> Msg item -> Model -> ( Model, Cmd msg )
update config message model =
    let
        andThenUpdate msg =
            andThen (update config msg)
    in
    case message of
        NoOp ->
            pure model

        ActiveElementParentIds ids ->
            pure model
                |> (if List.member config.domId ids then
                        identity

                    else
                        andThenUpdate Close
                   )

        SetOpen bool ->
            pure { model | open = bool }

        Close ->
            pure model |> andThenUpdate (SetOpen False)

        SelectClicked ->
            pure model |> andThenUpdate (SetOpen <| not model.open)

        ItemClicked item ->
            pure model
                |> andThenUpdate Close
                |> addMsgCmd (config.onSelect item)

        OnFocusOut ->
            pure model



{- |> nextTick (config.toMsg Close) -}


view : Config msg item -> Maybe item -> List item -> Model -> Html (Msg item)
view config maybeSelectedItem items model =
    let
        displayName =
            maybeSelectedItem
                |> Maybe.map config.toLabel
                |> Maybe.withDefault "<No Selection>"
    in
    div
        [ id config.domId
        , class "relative"
        , onFocusOut OnFocusOut
        ]
        [ div [ class "flex flex-row" ]
            [ button
                [ onClick SelectClicked
                , class "pa0 ma0 color-inherit flex items-center justify-center "
                ]
                [ div [ class "ttu" ] [ text displayName ]
                , FeatherIcons.chevronDown |> FeatherIcons.toHtml []
                ]
            ]
        , div
            [ class "absolute pv1 bg-white ba b--moon-gray shadow-1"
            , classList [ ( "dn", not model.open ) ]
            ]
            (List.map (viewItem config maybeSelectedItem) items)
        ]


viewItem : Config msg item -> Maybe item -> item -> Html (Msg item)
viewItem config maybeSelectedItem item =
    let
        isSelected =
            maybeSelectedItem |> unwrapMaybe False (eqs item)
    in
    button
        [ class "db f5 normal ttu hover-bg-lightest-blue pa0 ma0 color-inherit"
        , style "min-width" "8rem"
        , onClick <| ItemClicked item
        ]
        [ txtA
            [ class "ph3 pv2 "
            , classList [ ( "b", isSelected ) ]
            ]
            (config.toLabel item)
        ]
