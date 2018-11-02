module SelectUI exposing (Model, Msg(..), Option, OptionValue, closed, view)

import BasicsX exposing (..)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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
    div [ class "flex flex-row hs2" ]
        [ div [] [ text displayName ]
        , button
            [ onClick NoOp
            , class "flex items-center justify-center pa0 ma0"
            ]
            [ FeatherIcons.arrowDownCircle |> FeatherIcons.toHtml [] ]
        ]
