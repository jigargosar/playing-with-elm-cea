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
    | Selected OptionValue


type alias Config msg =
    { onSelect : OptionValue -> msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        NoOp ->
            pure model

        SelectClicked ->
            pure { model | open = not model.open }

        Selected item ->
            pure { model | open = False }
                |> perform config.onSelect (Task.succeed item)


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
    div [ class "" ]
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
                (List.map (\o -> txtA [ onClick <| Selected o.value ] o.name) options)
        ]
