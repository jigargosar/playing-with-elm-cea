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
import Browser.Events
import Debouncer exposing (Debouncer)
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
    , debouncer : Debouncer
    , documentHasFocus : Bool
    }


new =
    { open = False, debouncer = Debouncer.init, documentHasFocus = True }


type alias ShouldClose =
    Bool


type alias BounceMsg item =
    Maybe (Msg item)


type Msg item
    = NoOp
    | SelectClicked
    | ItemClicked item
    | OnFocusOut
    | OnFocusIn
    | DebouncedClose
    | DebouncerMsg (Debouncer.Msg (BounceMsg item))
    | DocumentFocusChanged Bool


type alias Config msg item =
    { onSelect : item -> msg
    , toMsg : Msg item -> msg
    , toLabel : item -> String
    , domId : DomId
    }


subscriptions : { x | toMsg : Msg item -> msg } -> Model -> Sub msg
subscriptions config model =
    Sub.batch
        [ Port.documentFocusChanged DocumentFocusChanged
        ]
        |> Sub.map config.toMsg


debouncerConfig : Debouncer.Config (Msg item) (Maybe (Msg item))
debouncerConfig =
    { toMsg = DebouncerMsg
    , wait = 0
    , onEmit = unwrapMaybe NoOp identity
    }


update : Config msg item -> Msg item -> Model -> ( Model, Cmd msg )
update config message model =
    let
        andThenUpdate msg =
            andThen (update config msg)

        andThenBounce maybeMsg =
            andThenUpdate <| DebouncerMsg <| Debouncer.bounce <| maybeMsg

        andThenBounceClose =
            andThenBounce <| Just DebouncedClose

        andThenCancelBounce =
            andThenBounce Nothing

        setOpen bool =
            setModel { model | open = bool }

        open =
            setOpen True

        close =
            setOpen False

        toggleOpen =
            setOpen (not model.open)
    in
    (case message of
        NoOp ->
            identity

        DebouncedClose ->
            close

        SelectClicked ->
            toggleOpen

        ItemClicked item ->
            close >> addMsg (config.onSelect item)

        DebouncerMsg msg ->
            andThen
                (updateSub (Debouncer.update debouncerConfig)
                    .debouncer
                    (\s b -> { b | debouncer = s })
                    msg
                    >> mapCmd config.toMsg
                )

        OnFocusOut ->
            andThenBounceClose

        OnFocusIn ->
            andThenCancelBounce

        DocumentFocusChanged hasFocus ->
            if model.open && not hasFocus then
                andThenCancelBounce

            else
                identity
    )
    <|
        pure model


view : Config msg item -> Maybe item -> List item -> Model -> Html msg
view config maybeSelectedItem items model =
    viewInternal config maybeSelectedItem items model
        |> Html.map config.toMsg


viewInternal : Config msg item -> Maybe item -> List item -> Model -> Html (Msg item)
viewInternal config maybeSelectedItem items model =
    let
        displayName =
            maybeSelectedItem
                |> Maybe.map config.toLabel
                |> Maybe.withDefault "<No Selection>"
    in
    div
        [ id config.domId
        , class "relative"
        , onFocusIn OnFocusIn
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
