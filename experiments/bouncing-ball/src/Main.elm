port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Return
import Svg
import Svg.Attributes as SA



---- PORT ----


port cache : E.Value -> Cmd msg



---- MODEL ----


type alias Flags =
    { cache : E.Value }


type alias CacheModel =
    { bgColor : String
    , ballColor : String
    }


type alias Model =
    { bgColor : String
    , ballColor : String
    , ballXY : ( Int, Int )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        { bgColor, ballColor } =
            D.decodeValue
                (D.map2 CacheModel
                    (D.field "bgColor" D.string)
                    (D.field "ballColor" D.string)
                )
                flags.cache
                |> Result.mapError (Debug.log "Error decoding flags.cache")
                |> Result.withDefault (CacheModel "#adbeeb" "#cd37a9")
    in
    update Cache (Model bgColor ballColor ( 100, 100 ))



---- UPDATE ----


type Msg
    = NoOp
    | BgColor String
    | BallColor String
    | Cache


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        BgColor bgColor ->
            ( { m | bgColor = bgColor }, Cmd.none )

        BallColor ballColor ->
            ( { m | ballColor = ballColor }, Cmd.none )

        Cache ->
            ( m, cacheModel m )


cacheModel { ballColor, bgColor } =
    E.object
        [ ( "ballColor", E.string ballColor )
        , ( "bgColor", E.string bgColor )
        ]
        |> cache



---- VIEW ----
---- Step 1 draw svg ----
-- * Make it pretty


green =
    "#b1ebad"


blue =
    "#4427d9"


svgView { bgColor, ballColor } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill bgColor ] []
        , Svg.circle
            [ SA.cx "100", SA.cy "100", SA.r (ballRadius |> String.fromInt), SA.fill ballColor ]
            []
        ]


viewColors { ballColor, bgColor } =
    H.div [ HA.class "hs3" ]
        [ H.input [ HA.type_ "color", HE.onInput BgColor, HA.value bgColor ] []
        , H.span [] [ H.text bgColor ]
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


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
