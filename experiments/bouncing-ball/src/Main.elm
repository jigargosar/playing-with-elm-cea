module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Model =
    { bgc : String
    , bc : String
    }


init : ( Model, Cmd Msg )
init =
    ( { bgc = green, bc = blue }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | BGC String
    | BC String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        BGC bgc ->
            ( { m | bgc = bgc }, Cmd.none )

        BC bc ->
            ( { m | bc = bc }, Cmd.none )



---- VIEW ----
---- Step 1 draw svg ----
-- * Make it pretty


green =
    "#b1ebad"


blue =
    "#4427d9"


svgView { bgc, bc } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill bgc ] []
        , Svg.circle
            [ SA.cx "100", SA.cy "100", SA.r (ballRadius |> String.fromInt), SA.fill bc ]
            []
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , H.div [ HA.class "hs3" ]
                [ H.input [ HA.type_ "color", HE.onInput BC, HA.value model.bgc ] []
                , H.text model.bgc
                , H.input [ HA.type_ "color", HE.onInput BC, HA.value model.bc ] []
                , H.text model.bc
                ]
            , H.div [ HA.class "" ] [ svgView model ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
