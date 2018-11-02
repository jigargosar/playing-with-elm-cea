module SelectUI exposing
    ( Model
    , Msg(..)
    , Option
    , OptionValue
    , Options
    , new
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
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    }


new =
    { open = False }


type alias OptionValue =
    String


type alias Option =
    { name : String, value : OptionValue }


type alias Options =
    List Option


type Msg
    = NoOp
    | SelectClicked
    | ItemClicked OptionValue
    | OnFocusOut
    | Close
    | SetOpen Bool


type alias Config msg =
    { onSelect : OptionValue -> msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    let
        andThenUpdate msg =
            andThen (update config msg)
    in
    case message of
        NoOp ->
            pure model

        SetOpen bool ->
            pure { model | open = bool }

        Close ->
            pure model |> andThenUpdate (SetOpen False)

        SelectClicked ->
            pure model |> andThenUpdate (SetOpen <| not model.open)

        ItemClicked item ->
            pure model
                |> andThenUpdate Close
                |> perform config.onSelect (Task.succeed item)

        OnFocusOut ->
            pure model |> andThenUpdate Close


view : Maybe OptionValue -> Options -> Model -> Html Msg
view maybeSelectedValue options model =
    let
        displayName =
            maybeSelectedValue
                |> Maybe.andThen
                    (\selectedValue ->
                        List.filter (.value >> eqs selectedValue) options
                            |> List.head
                            |> Maybe.map .name
                    )
                |> Maybe.withDefault "<No Selection>"
    in
    div
        [ id "select-context-ui"
        , class "relative"
--        , onFocusOut OnFocusOut
        ]
        [ div [ class "flex flex-row" ]
            [ button
                [ onClick SelectClicked
                , class "pa0 ma0 flex items-center justify-center color-inherit"
                ]
                [ div [ class "ttu" ] [ text displayName ]
                , FeatherIcons.chevronDown |> FeatherIcons.toHtml []
                ]
            ]
        , boolHtml model.open <|
            div [ class "absolute bg-white ba pa3 vs2" ]
                (List.map (\o -> txtA [ onClick <| ItemClicked o.value ] o.name) options)
        ]
