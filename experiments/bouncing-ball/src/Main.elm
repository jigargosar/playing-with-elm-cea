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
    , ballColor : String
    , ballXY : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { bgc = "#adbeeb"
      , ballColor = "#cd37a9"
      , ballXY = ( 100, 100 )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | BGC String
    | BallColor String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        BGC bgc ->
            ( { m | bgc = bgc }, Cmd.none )

        BallColor ballColor ->
            ( { m | ballColor = ballColor }, Cmd.none )



---- VIEW ----
---- Step 1 draw svg ----
-- * Make it pretty


green =
    "#b1ebad"


blue =
    "#4427d9"


svgView { bgc, ballColor } =
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
            [ SA.cx "100", SA.cy "100", SA.r (ballRadius |> String.fromInt), SA.fill ballColor ]
            []
        ]


viewColors { ballColor, bgc } =
    H.div [ HA.class "hs3" ]
        [ H.input [ HA.type_ "color", HE.onInput BGC, HA.value bgc ] []
        , H.span [] [ H.text bgc ]
        , H.input [ HA.type_ "color", HE.onInput BallColor, HA.value ballColor ] []
        , H.span [] [ H.text ballColor ]
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewColors model
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
