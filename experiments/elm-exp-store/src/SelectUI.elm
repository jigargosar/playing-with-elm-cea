module SelectUI exposing (Model, Msg(..), Option, OptionValue, Options, closed, view)

import BasicsX exposing (..)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { open : Bool
    }


closed =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        SelectClicked ->
            pure { model | open = not model.open }


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
    div [ class "flex flex-row" ]
        [ button
            [ onClick SelectClicked
            , class "pa0 ma0 flex items-center justify-center color-inherit"
            ]
            [ div [ class "ttu pl2" ] [ text displayName ]
            , FeatherIcons.chevronDown |> FeatherIcons.toHtml []
            ]
        ]
