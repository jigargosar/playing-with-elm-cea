module SelectUI exposing (Model, Msg(..), OptionValue, Options, ViewConfig, view)

import BasicsX exposing (..)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { open : Bool
    , selectedValue : Maybe OptionValue
    }


type alias OptionValue =
    String


type alias Options =
    { name : String, value : OptionValue }


type alias ViewConfig =
    { options : List Options
    }


type Msg
    = NoOp


view : ViewConfig -> Model -> Html Msg
view config model =
    let
        displayName =
            model.selectedValue
                |> Maybe.andThen
                    (\selectedValue ->
                        List.filter (.value >> eqs selectedValue) config.options
                            |> List.head
                            |> Maybe.map .name
                    )
                |> Maybe.withDefault "<No Selection>"
    in
    div [ class "flex flex-row hs2" ]
        [ div [] [ text displayName ]
        , button
            [ onClick NoOp
            , class "flex items-center justify-center pa0 ma0"
            ]
            [ FeatherIcons.arrowDownCircle |> FeatherIcons.toHtml [] ]
        ]
